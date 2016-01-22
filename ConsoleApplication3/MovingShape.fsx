#load "MIDLWC.fsx"

open MIDLWC
open System.Windows.Forms
open System.Drawing

type IUMLineMove() =
    inherit LWC()

    override this.HitTest (p: PointF) : bool =     
        false              
    override this.OnPaint(e) =
        let g = e.Graphics
        let pen = new Pen(this.Brush,float32 this.dimTickness)
        g.DrawLine(pen, PointF(0.f,0.f), PointF(this.Size.Width,this.Size.Height))       



type IUMEllipseMove() =
    inherit LWC()  
           
    override this.HitTest (p: PointF) : bool =     
        false              
    override this.OnPaint(e) =
        let g = e.Graphics
        let pen = new Pen(this.Brush, float32 this.dimTickness)
        g.DrawEllipse(pen, 0.f,0.f, this.Size.Width,this.Size.Height)  
             
type IUMMovementEllipseMove() =
    inherit LWC()  
           
    override this.HitTest (p: PointF) : bool =     
        false              
    override this.OnPaint(e) =
        let g = e.Graphics
        let pen = new Pen(this.Brush, float32 this.dimTickness)
        g.DrawEllipse(pen, 0.f,0.f, this.Size.Width,this.Size.Height)       

type IUMRectangleMove() =
    inherit LWC()

    override this.HitTest (p: PointF) : bool =     
        false              
    override this.OnPaint(e) =
        let g = e.Graphics
        let pen = new Pen(this.Brush,float32 this.dimTickness)
        let w,h = this.Size.Width, this.Size.Height
        let x, w = if w < 0.f then w, -w else 0.f, w
        let y, h = if h < 0.f then h, -h else 0.f, h
        
        g.DrawRectangle(pen, x, y, w, h)

