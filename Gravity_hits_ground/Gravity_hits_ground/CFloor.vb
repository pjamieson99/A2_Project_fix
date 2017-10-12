Public Class CFloor
    Public ypos As Integer
    Public Sub New(y As Integer)
        'set property
        ypos = y
    End Sub
    'draw floor
    Public Sub draw(g As Graphics)
        g.DrawLine(Pens.Black, 0, ypos, 10000, ypos)
    End Sub
End Class
