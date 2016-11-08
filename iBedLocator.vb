'
' IXXAT Automation GmbH
' 
' Demonstration source code how to use the VCI V3 with Visual Baisc 2005 
'
' Version 1.1.1.0 2009-02-27 (WuP)
'
' This is a beta release where are some things not ready (like comments)
' And some things can be programmed more elegantly
'
' The Version 0.2 includes a listcontrol to show the available interfaces
' A own thread wait for a event that some interface has removed or added
' (this happens typically with USB interfaces)
'
' The version 0.3 shows how to read the unique hardware ID of the device
' and show it as string on a label after selecting a device. A bug in
' the function "SelectDevice" removed, MoveNext was called 2 times.
'
' The version 0.4 shows how to convert the uniques hardware ID to a 
' human readable serial number, see function "GetSerialNumber"
'
' Version 0.5: Fixed some comments. Use ASCII for encoding the serial number.
' Add a function to set an acceptance filter to bit 0. Remove a dispose call
' in the write data function.
'
' Version 1.0.0.0: Show error frames. Show the version number of the assembly.
'
' Version 1.1.0.0 2008-10-31
' Change the vcinet2 reference path to the global 
' assembly cache. Add a baud rate list box. Set some objects to nothing
' after closing it to prevent disposing of a disposed object.
'
' Version 1.1.1.0 2009-02-27
' Set in the interface change thread the WaitOne function to wait infinite.
' Before it was 0, that causes 100% CPU load.
'
' Version 1.2.2.0 2011-04-26
' Add a project file to compile with Visual Basic .NET 2010 Express
' Change the edit field background to "window text"
'
' www.ixxat.de
' support@ixxat.de
'
'
'Eric Pietrowicz 
'12/7/15
'IR Scanner for iBED Bed Bay ID Identifier
' 

Imports System
Imports System.Threading

Public Class FormVCIV3Demo

    Dim rawDataString As String
    Dim bbidString As String = ""
    Dim counter As Integer = 0
    Dim counter1 As Integer = 0

    Private bbidList As List(Of String)

    Private mDevice As Ixxat.Vci3.IVciDevice = Nothing
    Private mCanChn As Ixxat.Vci3.Bal.Can.ICanChannel = Nothing
    Private mReader As Ixxat.Vci3.Bal.Can.ICanMessageReader = Nothing
    Private mWriter As Ixxat.Vci3.Bal.Can.ICanMessageWriter = Nothing
    Private mCanCtl As Ixxat.Vci3.Bal.Can.ICanControl = Nothing

    Private mRxEvent As System.Threading.AutoResetEvent = Nothing
    Private rxThread As System.Threading.Thread = Nothing

    Private deviceManager As Ixxat.Vci3.IVciDeviceManager = Nothing
    Private deviceList As Ixxat.Vci3.IVciDeviceList = Nothing
    Private deviceEnum As IEnumerator = Nothing

    Private changeEvent As New AutoResetEvent(True)
    Private interfaceChangeThread As System.Threading.Thread = Nothing


    ' This delegate enables asynchronous calls for setting
    ' the text property on a TextBox control.
    Delegate Sub SetTextCallback(ByVal [text] As String)

    ' Here is the tread safe call for the interface list box
    Delegate Sub FillListBoxWithInterFacesCallBack()

    
    Private Sub CloseAll()
        If mCanCtl IsNot Nothing Then
            ' stop the CAN controller
            Try
                mCanCtl.StopLine()
            Catch ex As Exception
                ' the can control has leave the scope,
                ' e.g. a plugable interface which was removed
            End Try
        End If

        TimerGetStatus.Enabled = False

        If rxThread IsNot Nothing Then
            '
            ' tell receive thread to quit
            '
            rxThread.Abort()

            '
            ' Wait for termination of receive thread
            '
            rxThread.Join()

            rxThread = Nothing
        End If

        If interfaceChangeThread IsNot Nothing Then
            '
            ' tell interface change thread to quit
            '
            interfaceChangeThread.Abort()

            '
            ' Wait for termination of interface change thread
            '
            interfaceChangeThread.Join()

            interfaceChangeThread = Nothing
        End If

        ' dispose all open objects including the vci object itself
        CloseVciObjects(True)
    End Sub


    Private Sub ButtonClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonClose.Click

        CloseAll()

        Close()
    End Sub

    Private Sub ButtonInit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonInit.Click
        ResetBBIDTimer.Enabled = True
        ResetBBIDTimer.Start()
        'System.Windows.Forms.Cursor.Current = Cursors.WaitCursor




        Dim CurListViewItem As ListViewItem
        CurListViewItem = ListViewAvailInterfaces.SelectedItems(0)

        TimerGetStatus.Enabled = False

        CloseCurrentExistingController()

        If CurListViewItem IsNot Nothing Then


            SelectDevice(CurListViewItem.Tag)
            InitSocket(0)

            '
            ' start the receive thread
            '
            rxThread = New System.Threading.Thread(New System.Threading.ThreadStart(AddressOf ReceiveThreadFunc))
            rxThread.Start()


            ButtonInit.Enabled = False
            'ButtonTransmitData.Enabled = True
            TimerGetStatus.Enabled = True

            '      TimerSendData.Enabled = True

        End If  'If CurListViewItem
    End Sub

    Private Sub SelectDevice(ByVal DeviceEnumNumber As Long)
        Dim mTempDevice As Ixxat.Vci3.IVciDevice = Nothing
        Dim deviceHardwareID As Object
        Try
            deviceEnum = deviceList.GetEnumerator()
            deviceEnum.Reset()

            Do While deviceEnum.MoveNext() = True

                mTempDevice = deviceEnum.Current

                If mTempDevice.VciObjectId = DeviceEnumNumber Then
                    mDevice = mTempDevice

                    ' a sample how to read the unique hardware ID from the device
                    deviceHardwareID = mTempDevice.UniqueHardwareId

                    ' if the UniqueHardwareId returns a GUID the try to convert it
                    ' to a IXXAT like serial number HWxxxxxx
                    If TypeOf deviceHardwareID Is System.Guid Then
                        LblHWID.Text = GetSerialNumber(deviceHardwareID)
                    Else
                        LblHWID.Text = deviceHardwareID.ToString()
                    End If

                End If

            Loop

        Catch ex As Exception
            ' todo show the exception
        End Try

    End Sub


    Private Sub InitSocket(ByVal canNo As Byte)
        Dim bal As Ixxat.Vci3.Bal.IBalObject

        Dim balType As Type
        balType = GetType(Ixxat.Vci3.Bal.Can.ICanChannel)


        Try
            bal = mDevice.OpenBusAccessLayer()

            mCanChn = bal.OpenSocket(canNo, balType)

            ' Initialize the message channel
            mCanChn.Initialize(1024, 128, False)

            ' Get a message reader object
            mReader = mCanChn.GetMessageReader()

            ' Initialize message reader
            mReader.Threshold = 1

            ' Create and assign the event that's set if at least one message
            ' was received.
            mRxEvent = New System.Threading.AutoResetEvent(False)
            mReader.AssignEvent(mRxEvent)

            ' Get a message writer object
            mWriter = mCanChn.GetMessageWriter()

            ' Initialize message writer
            mWriter.Threshold = 1

            ' Activate the message channel
            mCanChn.Activate()


            ' Open the CAN controller
            Dim canCtrlType As Type
            canCtrlType = GetType(Ixxat.Vci3.Bal.Can.ICanControl)
            mCanCtl = bal.OpenSocket(canNo, canCtrlType)

            ' Initialize the CAN controller
            Dim operatingMode As Byte
            operatingMode = Ixxat.Vci3.Bal.Can.CanOperatingModes.Standard Or Ixxat.Vci3.Bal.Can.CanOperatingModes.Extended Or Ixxat.Vci3.Bal.Can.CanOperatingModes.ErrFrame


            Dim bitRate As Ixxat.Vci3.Bal.Can.CanBitrate
            bitRate = GetSelectedBaudRate()
            mCanCtl.InitLine(operatingMode, bitRate)

            '  Set the acceptance filter
            Dim accCode As UInteger
            Dim accMask As UInteger

            accCode = Ixxat.Vci3.Bal.Can.CanAccCode.All
            accMask = Ixxat.Vci3.Bal.Can.CanAccMask.All

            mCanCtl.SetAccFilter(Ixxat.Vci3.Bal.Can.CanFilter.Std, accCode, accMask)

            ' Start the CAN controller
            mCanCtl.StartLine()
        Catch ex As Exception
            MessageBox.Show(ex.ToString())
            Return
        End Try


    End Sub

    ' if another controller is selected then the
    ' existing controller and channel objects must be closed
    Private Sub CloseCurrentExistingController()
        ' if necessary close a existing connection
        If mCanCtl IsNot Nothing Then
            ' stop the CAN controller
            Try
                mCanCtl.StopLine()
            Catch ex As Exception
                ' the can control has leave the scope,
                ' e.g. a plugable interface which was removed
            End Try
        End If

        ' close the receive thread we will reopen it
        ' with another event
        If rxThread IsNot Nothing Then
            '
            ' tell receive thread to quit
            '
            rxThread.Abort()

            '
            ' Wait for termination of receive thread
            '
            rxThread.Join()

            rxThread = Nothing
        End If
        CloseVciObjects(False)
    End Sub

    Private Sub CloseVciObjects(closeVciObject As Boolean)
        ' Dispose all hold VCI objects.

        ' Dispose message reader
        If (mReader IsNot Nothing) Then
            DisposeVciObject(mReader)
            mReader = Nothing
        End If

        ' Dispose message writer 
        If (mWriter IsNot Nothing) Then
            DisposeVciObject(mWriter)
            mWriter = Nothing
        End If

        ' Dispose CAN channel
        If (mCanChn IsNot Nothing) Then
            DisposeVciObject(mCanChn)
            mCanChn = Nothing
        End If

        ' Dispose CAN controller
        If (mCanCtl IsNot Nothing) Then
            DisposeVciObject(mCanCtl)
            mCanCtl = Nothing
        End If

        ' Dispose VCI device
        DisposeVciObject(mDevice)
    End Sub

    Private Sub DisposeVciObject(ByVal obj As Object)
        If obj IsNot Nothing Then
            Dim dispose As System.IDisposable
            dispose = obj
            If dispose IsNot Nothing Then
                dispose.Dispose()
                obj = Nothing
            End If
        End If
    End Sub

    Private Sub SetText(ByVal [text] As String)

        ' InvokeRequired required compares the thread ID of the
        ' calling thread to the thread ID of the creating thread.
        ' If these threads are different, it returns true.
        If labelLastRxMsg.InvokeRequired Then
            Dim d As New SetTextCallback(AddressOf SetText)
            Me.Invoke(d, New Object() {[text]})
        Else
            labelLastRxMsg.Text = [text]
        End If
    End Sub


    ' a thread callback function to check if the 
    ' interfaces have changed (e.g. removed or added a USB interface)
    Private Sub InterfaceChangeThreadFunc()
        Do
            If changeEvent.WaitOne(-1, False) Then
                FillListBoxWithInterFaces()
            End If
        Loop
    End Sub


    Private Sub ReceiveThreadFunc()

        Dim canMessage As Ixxat.Vci3.Bal.Can.CanMessage
        Do
            ' Wait 100 msec for a message reception
            If mRxEvent.WaitOne(100, False) Then

                '      // read a CAN message from the receive FIFO
                If mReader.ReadMessage(canMessage) Then
                    ShowReceivedMessage(canMessage)
                End If 'If mReader.ReadMessage(canMessage) Then
            End If  'If mRxEvent.WaitOne(100, False) Then
        Loop
    End Sub

    Private Sub ShowReceivedMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)
        Select Case canMessage.FrameType
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.Data
                ShowDataMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.Error
                ShowErrorMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.Info
                ShowInfoMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.Status
                ShowStatusMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.TimeOverrun
                ShowTimerOverrunMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.TimeReset
                ShowTimerResetMessage(canMessage)
            Case Ixxat.Vci3.Bal.Can.CanMsgFrameType.Wakeup
                ShowWakeUpMessage(canMessage)
        End Select
    End Sub



    Private Sub ShowErrorMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)
        ' todo
        Dim msgError As Ixxat.Vci3.Bal.Can.CanMsgError
        msgError = canMessage(0)
        Select Case msgError
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Acknowledge
                SetText("Error: Acknowledge")
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Bit
                SetText("Error: Bit")
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Crc
                SetText("Error: Crc")
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Form
                SetText("Error: Form")
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Other
                SetText("Error: Frequency not detected")
            Case Ixxat.Vci3.Bal.Can.CanMsgError.Stuff
                SetText("Error: Frequency not detected")
        End Select
    End Sub
    Private Sub ShowInfoMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)

    End Sub
    Private Sub ShowStatusMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)

    End Sub
    Private Sub ShowTimerOverrunMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)
        ' todo
    End Sub
    Private Sub ShowTimerResetMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)
        ' todo
    End Sub
    Private Sub ShowWakeUpMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)
        ' todo
    End Sub

    Private Sub FillListBoxWithInterFaces()

        If ListViewAvailInterfaces.InvokeRequired Then
            Dim d As New FillListBoxWithInterFacesCallBack(AddressOf FillListBoxWithInterFaces)
            Me.Invoke(d, New Object() {})
        Else
            ' first remove all items of the listbox
            ' ListBoxAvailInterfaces.Items.Clear()
            ListViewAvailInterfaces.Items.Clear()

            '    Dim CurItem As Item
            'Dim CurItemIndex As Integer

            ' now walk through the device list
            Try
                deviceManager = Ixxat.Vci3.VciServer.GetDeviceManager()
                deviceList = deviceManager.GetDeviceList()
                deviceEnum = deviceList.GetEnumerator()
                deviceEnum.Reset()
                'WuP: todo
                ' deviceList.AssignEvent(changeEvent)
                Do While deviceEnum.MoveNext() = True
                    'while deviceEnum.MoveNext() = True Then
                    mDevice = deviceEnum.Current


                    ' set the new list view item
                    ' the Tag should be the unique object ID
                    ' the Text should be the device description
                    Dim CurListViewItem = New ListViewItem
                    CurListViewItem.Tag = mDevice.VciObjectId
                    CurListViewItem.Text = mDevice.Description

                    ListViewAvailInterfaces.Items.Add(CurListViewItem)
                Loop


            Catch ex As Exception
            End Try
        End If



    End Sub

    Private Sub FormVCIV3Demo_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        BreakOn()
        BreakOff()


        'initalize the list
        bbidList = New List(Of String)

        Text = Text + " V" + Application.ProductVersion
        deviceManager = Ixxat.Vci3.VciServer.GetDeviceManager()
        deviceList = deviceManager.GetDeviceList()

        ' set a event to the devicelist to send this event if the interface list changes
        changeEvent = New System.Threading.AutoResetEvent(False)
        deviceList.AssignEvent(changeEvent)

        FillListBoxWithInterFaces()



        ' set 500 kBit/s as default baud raet
        ListBoxBaudrate.SelectedIndex = 0

        ' start a own thread which wait for a interface change message e.g. a USB device was plugged in or out
        interfaceChangeThread = New System.Threading.Thread(New System.Threading.ThreadStart(AddressOf InterfaceChangeThreadFunc))
        interfaceChangeThread.Start()

    End Sub


    Private Sub ListViewAvailInterfaces_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListViewAvailInterfaces.SelectedIndexChanged
        ButtonInit.Enabled = True
        ShowDeviceHardwareID()
    End Sub

    Private Sub ShowDeviceHardwareID()
        Dim CurListViewItem As ListViewItem

        Dim selIndex As Windows.Forms.ListView.SelectedIndexCollection

        selIndex = ListViewAvailInterfaces.SelectedIndices()

        If selIndex.Count > 0 Then
            CurListViewItem = ListViewAvailInterfaces.SelectedItems(0)
            If CurListViewItem IsNot Nothing Then
                SelectDevice(CurListViewItem.Tag)
            End If
        End If
    End Sub


    ' Change the UniqueHardwareID to a string.
    ' Because of a bug in the .NET API unteil VCI 3.1.4.1784 the property
    ' UniqueHardwareId returns always a GUID. In newer version it returns
    ' a string with the HWxxxxx serial number.
    Private Function GetSerialNumber(ByVal inputGuid As System.Guid) As String
        Dim resultString As String

        ' Convert the GUID to a byte array
        Dim byteArray() As Byte = inputGuid.ToByteArray()

        ' The first 2 bytes must have HW as data, then it is really a serial number
        If (Chr(byteArray(0)) = "H") And (Chr(byteArray(1)) = "W") Then
            resultString = System.Text.Encoding.ASCII.GetString(byteArray)

        Else
            resultString = inputGuid.ToString()

        End If



        Return resultString
    End Function

    Private Function GetSelectedBaudRate() As Ixxat.Vci3.Bal.Can.CanBitrate

        Dim resultBaud As Ixxat.Vci3.Bal.Can.CanBitrate

        Select Case ListBoxBaudrate.SelectedIndex
            Case 0
                resultBaud = Ixxat.Vci3.Bal.Can.CanBitrate.Cia125KBit
            Case 1
                resultBaud = Ixxat.Vci3.Bal.Can.CanBitrate.Cia250KBit
            Case 2
                resultBaud = Ixxat.Vci3.Bal.Can.CanBitrate.Cia500KBit
            Case 3
                resultBaud = Ixxat.Vci3.Bal.Can.CanBitrate.Cia800KBit
            Case Else
                resultBaud = Ixxat.Vci3.Bal.Can.CanBitrate.Cia1000KBit
        End Select

        Return resultBaud
    End Function

    Private Sub FormVCIV3Demo_FormClosing(ByVal sender As System.Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles MyBase.FormClosing
        CloseAll()
    End Sub

    ' Set the acceptance mask that all identifiers can pass
    Private Sub btnAcceptanceAll_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        mCanCtl.StopLine()

        '  Set the acceptance filter
        Dim accCode As UInteger
        Dim accMask As UInteger

        accCode = Ixxat.Vci3.Bal.Can.CanAccCode.All
        accMask = Ixxat.Vci3.Bal.Can.CanAccMask.All

        mCanCtl.SetAccFilter(Ixxat.Vci3.Bal.Can.CanFilter.Std, accCode, accMask)

        mCanCtl.StartLine()

    End Sub

    ' Set the acceptance mask that all identifiers with bit 0 have value 1 can pass
    Private Sub btnAcceptanceID1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        mCanCtl.StopLine()

        '  Set the acceptance filter
        Dim accCode As UInteger
        Dim accMask As UInteger

        ' we want to see all identifier which have bit 0 set as 1
        ' binary mask/value filter:
        ' 000 0000 0001
        ' 000 0000 0001
        ' -------------
        ' xxx xxxx xxx1
        accCode = &H1
        accMask = &H1

        ' shift the identifier values one bit left, this means that the last
        ' bit (bit 0) is 0 and the RTR bit in the filter doesn't matter
        ' binary mask/value filter:
        ' 0000 0000 0010
        ' 0000 0000 0010
        ' --------------
        ' xxxx xxxx xx1x
        accCode = accCode << 1
        accMask = accMask << 1

        accCode = accCode + 1
        accMask = accMask + 1



        mCanCtl.SetAccFilter(Ixxat.Vci3.Bal.Can.CanFilter.Std, accCode, accMask)

        mCanCtl.StartLine()

    End Sub

    Private Sub setBBID()

    End Sub
    Private Sub ShowDataMessage(ByVal canMessage As Ixxat.Vci3.Bal.Can.CanMessage)

        rawDataString = "Time: " + canMessage.TimeStamp.ToString + " ID: " + canMessage.Identifier.ToString("X3") + "h"

        If canMessage.RemoteTransmissionRequest Then
            rawDataString = rawDataString + " Remote Request Data Length: " + canMessage.DataLength.ToString()
        Else
            Dim i As Byte
            For i = 1 To canMessage.DataLength
                'we start the data bytes from 0
                rawDataString = rawDataString + " " + canMessage.Data(i - 1).ToString("X2")
            Next
        End If

        If rawDataString.Substring(rawDataString.LastIndexOf("h") + 2).Length > 20 Then
            bbidString = rawDataString.Substring(rawDataString.LastIndexOf("h") + 2)
        End If


        



        If canMessage.SelfReceptionRequest Then
            rawDataString = rawDataString + " Self Reception"
        End If
        ' set the text thread safe to the label
        SetText(rawDataString)
    End Sub


    Private Sub ParseInput()

        Dim output As String = ""
        Dim nonZeroCheck As Boolean = bbidString.Contains("00 00 00 00")
        Dim checkOutput As Boolean = False

        Dim checkStatus As String = ""


        If bbidString <> "" Then
            output = bbidString.Substring(0, 17) 'filter out everything but the BBID
            output = output.Replace(" ", "") 'remove the spaces
        End If

        Dim charArray() As Char = bbidString.ToCharArray

        If charArray(19) = "2" Then
            BatteryIndicator.BackColor() = Color.LightGreen
            BatteryIndicator.Text = "GOOD"

        Else
            BatteryIndicator.BackColor() = Color.Red
            BatteryIndicator.Text = "POOR"
        End If

        If charArray(22) = "1" Then
            ConnectivityStatus.BackColor() = Color.LightGreen
            ConnectivityStatus.Text = "CONNECTED"
        Else
            ConnectivityStatus.BackColor() = Color.Red
            ConnectivityStatus.Text = "NOT CONNECTED"

        End If

        If bbidList.Contains(output) Then
            checkOutput = True
        End If
        If checkOutput = False And nonZeroCheck = False Then
            bbidList.Add(output)
        End If

        Dim sb As New System.Text.StringBuilder()
        Dim outputString As String = ""
        For Each bbid As String In bbidList
            'outputString = outputString.ToLower & vbCrLf & bbid.ToLower
            'outputString.
            ' outputString = bbid.ToLower
            outputString = sb.Append(bbid & vbCrLf).ToString().ToLower
        Next
        '
        bbidOutput.Text = outputString

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        ConnectivityTimer.Enabled = True
        ConnectivityTimer.Start()
        Button1.Enabled = False
        WMReconnectTimer.Enabled = True
        WMReconnectTimer.Start()
        'bbidListBox.Items.Add(outputString)
        'bbidOutput.Text = outputString

        bbidOutput.Text = ""
        BreakOn()
        BreakOff()

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        bbidList.Clear()
        ConnectivityTimer.Enabled = False
        ConnectivityTimer.stop()
        bbidOutput.Text = "" 'clear the output box


    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If bbidOutput.Text <> String.Empty Then
            Clipboard.SetText(bbidOutput.Text) 'copies the output BBID's to the computer's clipboard
        Else
            Clipboard.Clear()
        End If
    End Sub


    Private Sub BreakOn()

        If mWriter IsNot Nothing Then
            Dim canMessage As Ixxat.Vci3.Bal.Can.CanMessage

            canMessage.TimeStamp = 0
            canMessage.Identifier = 882 'Decimal value for 327 in Hex
            canMessage.FrameType = Ixxat.Vci3.Bal.Can.CanMsgFrameType.Data
            canMessage.DataLength = 8
            canMessage.SelfReceptionRequest = True
            canMessage.Data(0) = 2 'Bit location/data sent

            If mWriter.Capacity > 0 Then
                mWriter.SendMessage(canMessage)
            End If

        End If
    End Sub

    Private Sub BreakOff()

        If mWriter IsNot Nothing Then
            Dim canMessage As Ixxat.Vci3.Bal.Can.CanMessage

            canMessage.TimeStamp = 0
            canMessage.Identifier = 882 'Decimal value for 327 in Hex
            canMessage.FrameType = Ixxat.Vci3.Bal.Can.CanMsgFrameType.Data
            canMessage.DataLength = 8
            canMessage.SelfReceptionRequest = True
            canMessage.Data(0) = 0 'Bit location/data sent

            If mWriter.Capacity > 0 Then
                mWriter.SendMessage(canMessage)
            End If

        End If
    End Sub

    Private Sub ResetBBIDTimer_Tick(sender As Object, e As EventArgs) Handles ResetBBIDTimer.Tick

        counter = counter + 1
        If counter > 1 Then
            Button1.Enabled = True
            ResetBBIDTimer.Stop()
            ResetBBIDTimer.Enabled = False

        End If

    End Sub

    Private Sub WMReconnectTimer_Tick(sender As Object, e As EventArgs) Handles WMReconnectTimer.Tick

        counter1 = counter1 + 1
        If counter1 > 1 Then
            Button1.Enabled = True
            WMReconnectTimer.Stop()
            WMReconnectTimer.Enabled = False
            counter1 = 0

        End If


    End Sub

    Private Sub ConnectivityTimer_Tick(sender As Object, e As EventArgs) Handles ConnectivityTimer.Tick
        ParseInput()
    End Sub


    Private Sub SaveAs_Click(sender As Object, e As EventArgs) Handles SaveAs.Click
        SaveFileDialog1.ShowDialog()
        Dim saveFileText As String = SaveFileDialog1.FileName

        Dim streamw As New IO.StreamWriter(saveFileText)
        Dim i As Integer
        For i = 0 To bbidList.Count - 1
            streamw.WriteLine(bbidList.Item(i))
        Next
        streamw.Close()
        streamw.Dispose()
        MsgBox("Done")
    End Sub

    Private Sub SaveAsTxt_TextChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub SaveFileDialog1_FileOk(sender As Object, e As ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk

    End Sub
End Class

