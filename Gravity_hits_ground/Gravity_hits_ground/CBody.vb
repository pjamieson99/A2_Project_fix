Public Class CBody
    Private xpos1
    Private xpos2
    Private ypos1
    Private ypos2
    Public CoM As PointF
    Public yspeed As Integer
    Public connections(2) As PointF
    Public Left As Boolean
    Public Right As Boolean

    Public Middle() As PointF
    Public RaiseDistance As Double
    Public Sub New(x1, y1, x2, y2)

        ypos1 = y1
        xpos1 = x1
        ypos2 = y2
        xpos2 = x2
        CoM = New Point((x1 + x2) / 2, (y1 + y2) / 2)
        yspeed = 0

    End Sub

    Public Sub draw(g As Graphics)
        g.DrawLine(Pens.Black, xpos1, ypos1, xpos2, ypos2)
    End Sub

    Public Sub drop()
        'add speed to fall
        ypos1 += yspeed
        ypos2 += yspeed
        yspeed += 1
        For x = 0 To 2
            connections(x).Y += yspeed
        Next
    End Sub

    Sub BodyPoints(P1Legs() As PointF, GroundLegs As Integer)
        For x = 0 To GroundLegs
            connections(x) = P1Legs(x)
        Next

    End Sub
End Class
