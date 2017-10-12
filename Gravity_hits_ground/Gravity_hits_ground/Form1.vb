Public Class Form1
    Dim GroundLegs As Integer = 2
    Dim Layers As Integer = 1
    Dim Line(GroundLegs, Layers) As CLeg
    Dim floor As CFloor
    Dim Rnd As New Random
    Dim joint(GroundLegs, Layers) As CJoint
    Dim P1 As Integer = 100
    Dim down As Boolean = False
    Dim Up As Boolean = True
    Dim Body As CBody
    Dim P1Legs(GroundLegs) As PointF
    Dim LineDiff As Double


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'create floor object
        floor = New CFloor(500)
        Body = New CBody(100, 0, 300, 0)
        'create legs and joints
        For y = 0 To Layers
            If y = 1 Then
                P1 -= (GroundLegs + 1) * 100
            End If
            For x = 0 To GroundLegs
                If y = 1 Then
                    Line(x, y) = New CLeg(P1, 0, Rnd)
                    joint(x, y) = New CJoint(P1, 0, 10, 10, 1)
                    P1Legs(x) = Line(x, y).p1
                Else
                    Line(x, y) = New CLeg(P1, 100, Rnd)
                    joint(x, y) = New CJoint(P1, 100, 10, 10, 1)
                End If
                
                P1 += 100
            Next



        Next
        P1 -= 300
        Body.BodyPoints(P1Legs, GroundLegs)
    End Sub




    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'keep display refreshing, loop through display
        Display.Refresh()
    End Sub

    Private Sub Display_Paint(sender As Object, e As PaintEventArgs) Handles Display.Paint
        Dim g As Graphics
        g = e.Graphics

        'draw the floor
        floor.draw(g)

        For y = 0 To Layers
            For x = 0 To GroundLegs
                Line(x, y).AngleLock(floor)




                'If Line(x, y).py2 >= floor.ypos Then

                '    Line(x, y).Friction()
                'Else
                Line(x, y).NewPoints()

                'End If


            Next
        Next

        For y = 0 To Layers
            For x = 0 To GroundLegs
                If y = 0 Then
                    Line(x, y).px2 += Line(x, y + 1).px2 - Line(x, y).px1
                    Line(x, y).px1 = Line(x, y + 1).px2
                    Line(x, y).py2 += Line(x, y + 1).py2 - Line(x, y).py1
                    Line(x, y).py1 = Line(x, y + 1).py2
                End If
                If y = 0 Then
                    If floor.ypos <= Line(x, y).py1 Or floor.ypos <= Line(x, y).py2 Then
                        Line(x, y).Yspeed = 0

                        Line(x, y + 1).px1 += Line(x, y).px1 - Line(x, y + 1).px2
                        Line(x, y + 1).px2 = Line(x, y).px1
                        Line(x, y + 1).py1 += Line(x, y).py1 - Line(x, y + 1).py2
                        Line(x, y + 1).py2 = Line(x, y).py1
                        Line(x, y + 1).py1 -= (Line(x, y).py2 - floor.ypos)
                        Line(x, y + 1).LYpos -= (Line(x, y).py2 - floor.ypos)
                        Line(x, y + 1).py2 -= (Line(x, y).py2 - floor.ypos)
                    End If

                    If Line(x, y).Yspeed = 0 Then
                        Line(x, y + 1).Yspeed = 0
                    End If
                End If
                Line(x, 1).Oldpy1 = Line(x, 1).py1
                Line(x, y).HitFloor(floor)

                If Line(x, y).CheckFloor(floor) = True Then
                    Body.yspeed = 0

                End If

                joint(x, y).rise(Line(x, y).p1)
                Line(x, y).draw(g, floor)
                joint(x, y).draw(g)
                Line(x, y).drop()
                joint(x, y).drop(floor)

                'Line(x, y).Yspeed += 1
                'joint(x, y).Yspeed += 1
            Next
        Next


        For x = 0 To 2
            If Line(x, 0).CheckFloor(floor) And Line(x, 0).px2 < Body.CoM.X Then
                Body.Left = True
                Line(x, 1).LeftSide = True
                Line(x, 0).LeftSide = True
            ElseIf Line(x, 0).CheckFloor(floor) And Line(x, 0).px2 > Body.CoM.X Then
                Body.Right = True
                Line(x, 1).RightSide = True
                Line(x, 0).RightSide = True
            End If
        Next
        If Body.Left And Body.Right = False Then

        ElseIf Body.Left = False Or Body.Right = False Then
            'fall()
        End If

        For x = 0 To 2
            If Line(x, 1).py1 < Body.connections(x).Y And Line(x, 0).CheckFloor(floor) Then

                If Line(x, 1).LeftSide = True Then
                    For y = 0 To GroundLegs
                        If Line(GroundLegs - y, 0).RightSide = True And Line(GroundLegs - y, 0).CheckFloor(floor) = True Then
                            Body.RaiseDistance = Line(x, 1).px1 - Line(GroundLegs - y, 1).px1
                            LineDiff = Line(GroundLegs - y, 1).px1 - Line(x, 1).px1 + Math.Sqrt(Body.RaiseDistance ^ 2 - (Body.connections(x).Y - Line(x, 1).py1) ^ 2)
                            Line(GroundLegs - y, 1).px1 = Line(x, 1).px1 + Math.Sqrt(Body.RaiseDistance ^ 2 - (Body.connections(x).Y - Line(x, 1).py1) ^ 2)
                            Exit For
                        End If
                    Next


                End If

                If Line(x, 1).RightSide = True Then
                    For y = 0 To GroundLegs
                        If Line(y, 0).LeftSide = True And Line(y, 0).CheckFloor(floor) = True Then
                            Body.RaiseDistance = Line(x, 1).px1 - Line(GroundLegs - y, 0).px1
                            LineDiff = Line(GroundLegs - y, 1).px1 - Line(x, 1).px1 + Math.Sqrt(Body.RaiseDistance ^ 2 - (Body.connections(x).Y - Line(x, 1).py1) ^ 2)
                            Line(GroundLegs - y, 1).px1 = Line(x, 1).px1 + Math.Sqrt(Body.RaiseDistance ^ 2 - (Body.connections(x).Y - Line(x, 1).py1) ^ 2)
                            Exit For
                        End If
                    Next
                End If
                For y = 0 To GroundLegs
                    If y <> x Then
                        Line(y, 1).px1 += LineDiff
                        Body.connections(x).X = Line(y, 1).px1
                    End If
                Next





                Body.connections(x).Y = Line(x, 1).py1
            End If
        Next

        Body.draw(g)

        For x = 0 To GroundLegs
            Line(x, 1).p2.Y += Body.connections(x).Y - Line(x, 1).p1.Y
            Line(x, 1).py2 += Body.connections(x).Y - Line(x, 1).p1.Y
            Line(x, 1).p1 = Body.connections(x)
            Line(x, 1).LYpos = Line(x, 1).p1.Y
            Line(x, 1).py1 = Line(x, 1).p1.Y
        Next







        'For x = 0 To GroundLegs
        '    If line(x,0).px1 < Body.CoM.X And Line(x, 0).CheckFloor(floor) = True Then
        '        Body.Left = True
        '        Body.LeftSide(x) = Body.connections(x)
        '    ElseIf Body.connections(x).X > Body.CoM.X And Line(x, 0).CheckFloor(floor) = True Then
        '        Body.Right = True
        '        Body.RightSide(x) = Body.connections(x)
        '    ElseIf Body.connections(x).X = Body.CoM.X And Line(x, 0).CheckFloor(floor) = True Then
        '        Body.Left = True
        '        Body.Right = True
        '        Body.Middle(x) = Body.connections(x)
        '    End If
        'Next

        'If Body.Right = False Or Body.Left = False Then
        '    'Fall()
        'Else
        '    For x = 0 To GroundLegs
        '        If Line(x, 1).py1 < Body.connections(x).Y Then
        '            For y = 0 To GroundLegs
        '                'check if line is left or right, return left or right
        '            Next
        '            'if check is right then
        '            '
        '            'elseif check is left then
        '            'line(body.leftside().legnth -1 + body.middle().length + bodt.rightside().legnth-1, 1) = pivot
        '            '
        '            '
        '            'next
        '            'else
        '            'for x = 0 to groundlegs
        '            'body.connections(x).y +=  line(x,1).y - body.connections(x).y
        '            'body.ypos1 += 
        '            'body.ypos2 +=
        '            'next
        '            'end if
        '        End If
        '    Next
        'End If

        Body.drop()

    End Sub



    Private Sub Display_Click(sender As Object, e As EventArgs) Handles Display.Click

    End Sub

    'TODO: make joints always spawn one one side of leg, make sure that there is an end to the leg, make it so legs can connect to joint, 2 legs to one joint etc. make end legs have friction to move forwards. make body and centre of mass and if centre of mass is away from pivot that it falls - whole object rotates 



End Class
