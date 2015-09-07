'Cheryl Muetzel
'Babysitter Pay System
'09/04/2015

Public Class Form1
    'Declare the constant.
    Const START_TIME_TO_BEDTIME_RATE_Decimal = 12D
    Const BEDTIME_TO_MIDNIGHT_RATE_Decimal = 8D
    Const MIDNIGHT_TO_STOP_TIME_RATE_Decimal = 16D
    Const MILITARY_TIME_CONVERT_Decimal = 12D
    Const MIDNIGHT_DECIMAL = 720D
    Const MIDNIGHT_HH = 12D


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub StartTimeHHTextBox_TextChanged(sender As Object, e As EventArgs) Handles StartTimeHHTextBox.TextChanged

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles ClearButton.Click
        'Clear previous entries from the form.
        StartTimeHHTextBox.Clear()
        StartTimeMMTextBox.Clear()
        BedtimeHHTextBox.Clear()
        BedtimeMMTextBox.Clear()
        StopTimeHHTextBox.Clear()
        StopTimeMMTextBox.Clear()
        TotalPaymentTextBox.Clear()
        AMRadioButton.Checked = False
        PMRadioButton.Checked = False

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles CalculateButton.Click
        Dim StartTimeHHInteger, StartTimeMMInteger, BedtimeHHInteger, BedtimeMMInteger, StopTimeHHInteger, StopTimeMMInteger,
        StartTimeAccumInteger, FirstPayTierPayAmtInteger, BedtimeAccumInteger, FirstPayTierAmtInteger, EndTimeAccumInteger, SecondPayTierAmtInteger,
        SecondPayTierPayAmtInteger, ThirdPayTierAmtInteger, ThirdPayTierPayAmtInteger, GrandTotalPayAmtInteger As Integer

        Dim ValidDataBoolean As Boolean

        'Initialize variables
        ValidDataBoolean = True
        'PMRadioButton.Equals(False)
        'AMRadioButton.Equals(False)

        Try
            StartTimeHHInteger = Integer.Parse(StartTimeHHTextBox.Text)
            If StartTimeHHInteger < 5 Then
                MessageBox.Show("Start time must be after 5 pm")
                StartTimeHHTextBox.Focus()
                ValidDataBoolean = False
            End If
            Try
                StartTimeMMInteger = Integer.Parse(StartTimeMMTextBox.Text)
                If StartTimeMMInteger > 59 Then
                    MessageBox.Show("Start time minutes must be less than 60")
                    ValidDataBoolean = False
                    StartTimeMMTextBox.Focus()
                End If
                Try
                    BedtimeHHInteger = Integer.Parse(BedtimeHHTextBox.Text)
                    Try
                        BedtimeMMInteger = Integer.Parse(BedtimeMMTextBox.Text)
                        If BedtimeMMInteger > 59 Then
                            MessageBox.Show("Bed time minutes must be less than 60")
                            ValidDataBoolean = False
                            BedtimeMMTextBox.Focus()
                        End If

                        Try
                            StopTimeHHInteger = Integer.Parse(StopTimeHHTextBox.Text)
                            If (StopTimeHHInteger > 3 And StopTimeHHInteger < 12) And AMRadioButton.Checked Then
                                MessageBox.Show("Enter a stop time before 4 am")
                                ValidDataBoolean = False
                                StopTimeHHTextBox.Focus()
                            Else
                                If (StopTimeHHInteger = 12) And PMRadioButton.Checked Then
                                    MessageBox.Show("Enter a stop time before 4 am")
                                    PMRadioButton.Equals(False)
                                    ValidDataBoolean = False
                                    StopTimeHHTextBox.Focus()
                                End If
                            End If

                            Try
                                StopTimeMMInteger = Integer.Parse(StopTimeMMTextBox.Text)
                                If StopTimeMMInteger > 59 Then
                                    MessageBox.Show("Stop time minutes must be less than 60")
                                    ValidDataBoolean = False
                                    StopTimeMMTextBox.Focus()
                                End If

                                If Not PMRadioButton.Checked And Not AMRadioButton.Checked Then
                                    MessageBox.Show("Check AM button for stop hours after 11:59 pm  or PM Button for hours before midnight")
                                    ValidDataBoolean = False
                                    PMRadioButton.Focus()
                                End If





                                'Calculate pay per hour:  start to bedtime=$12, bedtime to midnight=$8, midnight to end=$16
                                'No pay for partial hours

                                If ValidDataBoolean.Equals(True) Then
                                    StartTimeAccumInteger = (StartTimeHHInteger * 60) + StartTimeMMInteger
                                    BedtimeAccumInteger = (BedtimeHHInteger * 60) + BedtimeMMInteger
                                    EndTimeAccumInteger = (StopTimeHHInteger * 60) + StopTimeMMInteger

                                    'Calculate rate for start time to bed time
                                    FirstPayTierAmtInteger = Math.Truncate((BedtimeAccumInteger - StartTimeAccumInteger) / 60)
                                    FirstPayTierPayAmtInteger = FirstPayTierAmtInteger * START_TIME_TO_BEDTIME_RATE_Decimal
                                    GrandTotalPayAmtInteger = GrandTotalPayAmtInteger + FirstPayTierPayAmtInteger

                                    'Calculate rate for bedtime to midnight
                                    If StopTimeHHInteger > 0 And StopTimeHHInteger < 4 Or StopTimeHHInteger = 12 Then
                                        SecondPayTierAmtInteger = Math.Truncate((MIDNIGHT_DECIMAL - BedtimeAccumInteger) / 60)
                                        SecondPayTierPayAmtInteger = SecondPayTierAmtInteger * BEDTIME_TO_MIDNIGHT_RATE_Decimal
                                    Else
                                        SecondPayTierAmtInteger = Math.Truncate((EndTimeAccumInteger - BedtimeAccumInteger) / 60)
                                        SecondPayTierPayAmtInteger = SecondPayTierAmtInteger * BEDTIME_TO_MIDNIGHT_RATE_Decimal
                                    End If

                                    GrandTotalPayAmtInteger = GrandTotalPayAmtInteger + SecondPayTierPayAmtInteger

                                    'Calculate rate for midnight to stop time
                                    If StopTimeHHInteger > 0 And StopTimeHHInteger < 4 Then
                                        EndTimeAccumInteger = EndTimeAccumInteger + MIDNIGHT_DECIMAL
                                        ThirdPayTierAmtInteger = Math.Truncate((EndTimeAccumInteger - MIDNIGHT_DECIMAL) / 60)
                                        ThirdPayTierPayAmtInteger = ThirdPayTierAmtInteger * MIDNIGHT_TO_STOP_TIME_RATE_Decimal
                                    Else
                                        If StopTimeHHInteger = 12 Then
                                            ThirdPayTierAmtInteger = Math.Truncate((EndTimeAccumInteger - MIDNIGHT_DECIMAL) / 60)
                                            ThirdPayTierPayAmtInteger = ThirdPayTierAmtInteger * MIDNIGHT_TO_STOP_TIME_RATE_Decimal
                                        Else
                                            ThirdPayTierAmtInteger = 0
                                        End If
                                    End If

                                    GrandTotalPayAmtInteger = GrandTotalPayAmtInteger + ThirdPayTierPayAmtInteger

                                    'Display the grand total payment
                                    TotalPaymentTextBox.Text = GrandTotalPayAmtInteger.ToString("C")
                                End If

                            Catch ex As Exception
                                MessageBox.Show("Stop time minutes must be numeric")
                                StopTimeMMTextBox.Focus()
                            End Try
                        Catch ex As Exception
                            If StopTimeHHInteger > 3 And StopTimeHHInteger < 12 And AMRadioButton.Checked Then
                                MessageBox.Show("Enter a stop time before 4 am")
                                StopTimeHHTextBox.Focus()
                            End If
                            'MessageBox.Show("Stop time hour must be numeric")
                            'StopTimeHHTextBox.Focus()
                        End Try
                    Catch ex As [FormatException]
                        MessageBox.Show("Bedtime minutes must be numeric")
                        BedtimeMMTextBox.Focus()

                    End Try
                Catch ex As [FormatException]
                    MessageBox.Show("Bedtime hour must be numeric")
                    BedtimeHHTextBox.Focus()
                End Try

            Catch ex As [FormatException]
                MessageBox.Show("Start time minutes must be numeric")
                StartTimeMMTextBox.Focus()
            End Try
        Catch ex As [FormatException]
            MessageBox.Show("Start time hour must be numeric")
            StartTimeHHTextBox.Focus()

        End Try
    End Sub

End Class
