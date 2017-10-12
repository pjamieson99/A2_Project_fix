Public Class CLeg
    Public LXpos As Integer
    Public LYpos As Integer
    Public Angle As Integer
    Public Speed As Integer
    Public clock As Integer
    Public p1 As PointF
    Public p2 As PointF
    Public px1 As Double
    Public px2 As Double
    Public py1 As Double
    Public py2 As Double
    Public px1Temp As Double
    Public px2Temp As Double
    Public py1Temp As Double
    Public py2Temp As Double
    Public Yspeed As Integer
    Public increase As Integer
    Public GoThroughClock As Integer
    Public up As Boolean = True
    Public down As Boolean = False
    Dim Hit As Boolean = False
    Public LeftSide As Boolean
    Public RightSide As Boolean
    Public Oldpy1 As Double
    Public Sub New(x As Integer, y As Integer, rnd As Random)
        'set properties
        LXpos = x
        LYpos = y
        px1 = LXpos
        py1 = LYpos
        px2 = LXpos
        py2 = LYpos + 100
        p1 = New Point(LXpos, LYpos)
        p2 = New Point(LXpos, LYpos + 100)
        Speed = rnd.Next(1, 10)
        clock = rnd.Next(1, 50)
        Angle = rnd.Next(45, 170)
        GoThroughClock = 0
        Yspeed = 0
    End Sub

    Public Sub draw(g As Graphics, floor As CFloor)
        'draw object

        g.DrawLine(Pens.Black, p1.X, p1.Y, p2.X, p2.Y)
        'reset points to what the original dshape is but in its current position to allow the rotation matrix to rotate from original angle


        p1.X = LXpos
        p1.Y = LYpos
        p2.X = LXpos
        p2.Y = LYpos + 100


    End Sub

    Public Sub drop()
        'add speed to fall
        'LYpos += Yspeed
        'p1.Y += Yspeed
        'p2.Y += Yspeed
        'py1 += Yspeed
        'py2 += Yspeed
    End Sub

    Sub NewPoints()
        If up = True Then
            'rotate points using rotation matrix
            increase += 1
            px1 = ((p1.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p1.Y - LYpos) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py1 = ((p1.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p1.Y - LYpos) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos
            px2 = ((p2.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p2.Y - LYpos) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py2 = ((p2.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p2.Y - LYpos) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos

        ElseIf down = True Then
            increase -= 1
            px1 = ((p1.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p1.Y - LYpos) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py1 = ((p1.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p1.Y - LYpos) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos
            px2 = ((p2.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p2.Y - LYpos) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py2 = ((p2.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p2.Y - LYpos) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos

        End If

    End Sub

    Sub AngleLock(floor As CFloor)
        If Angle / 2 <= Speed * increase Then
            GoThroughClock += 1
            If GoThroughClock >= clock Then
                GoThroughClock = 0
                down = True
                up = False
            Else
                down = False
                up = False
            End If
        ElseIf Angle / 2 <= Speed * -increase And increase < 0 Then
            GoThroughClock += 1
            If GoThroughClock >= clock Then
                GoThroughClock = 0
                down = False
                up = True
            Else
                down = False
                up = False
            End If
        End If
    End Sub

    Sub HitFloor(floor As CFloor)
        'stop falling once hit floor
        If floor.ypos <= py1 Or floor.ypos <= py2 Then
            Yspeed = 0

        End If

        'check if points hit floor, if they do then raise the object up
        If py1 > floor.ypos Then
            py2 -= (py1 - floor.ypos)
            LYpos -= (py1 - floor.ypos)
            py1 -= (py1 - floor.ypos)

        Else

        End If
        If py2 > floor.ypos Then
            py1 -= (py2 - floor.ypos)
            LYpos -= (py2 - floor.ypos)
            py2 -= (py2 - floor.ypos)

        Else

        End If



        p1.X = px1
        p1.Y = py1
        p2.X = px2
        p2.Y = py2

    End Sub

    Function CheckFloor(floor As CFloor)
        If floor.ypos <= py1 Or floor.ypos <= py2 Then
            Return True
        Else
            Return False

        End If
    End Function
    Sub Friction()
        If up = True Then
            'rotate points using rotation matrix
            LXpos = px2
            p2.X = px2
            p1.X = px2

            increase += 1
            px1 = ((p1.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p1.Y - (LYpos + 100)) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py1 = ((p1.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p1.Y - (LYpos + 100)) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos + 100
            px2 = ((p2.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p2.Y - (LYpos + 100)) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py2 = ((p2.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p2.Y - (LYpos + 100)) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos + 100

        ElseIf down = True Then
            LXpos = px2
            p2.X = px2
            p1.X = px2

            increase -= 1
            px1 = ((p1.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p1.Y - (LYpos + 100)) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py1 = ((p1.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p1.Y - (LYpos + 100)) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos + 100
            px2 = ((p2.X - LXpos) * Math.Cos(Speed * increase * Math.PI / 180)) + ((p2.Y - (LYpos + 100)) * Math.Sin(Speed * increase * Math.PI / 180)) + LXpos
            py2 = ((p2.X - LXpos) * (-Math.Sin(Speed * increase * Math.PI / 180))) + ((p2.Y - (LYpos + 100)) * Math.Cos(Speed * increase * Math.PI / 180)) + LYpos + 100
        End If
    End Sub


End Class
