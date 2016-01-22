
open System.Windows.Forms
open System.Drawing

type LWC() =
    let mutable parent : LWC option = None
    let mutable position = PointF(0.f,0.f)
    let mutable rotateAngle = 0.f
    let mutable scaleFactor = PointF(1.f,1.f)
    let mutable size = SizeF(800.f,800.f) 
    let mutable brush = Brushes.Black
    let mutable interested = false
    let mutable dimThickness = 5
    let mutable path : System.Drawing.Drawing2D.GraphicsPath option =  None
    
    
    let mutable v2wTransform =new Drawing2D.Matrix()
    let mutable w2vTransform =new Drawing2D.Matrix()

    abstract OnMouseDown : MouseEventArgs -> bool
    default this.OnMouseDown _ = false
    
    abstract OnKeyDown : KeyEventArgs -> unit
    default this.OnKeyDown _ = ()

    abstract OnKeyUp : KeyEventArgs -> unit
    default this.OnKeyUp _ = ()

    abstract OnMouseMove : MouseEventArgs -> bool
    default this.OnMouseMove _ = false

    abstract OnMouseUp : MouseEventArgs -> bool
    default this.OnMouseUp _ = false

    abstract OnPaint : PaintEventArgs -> unit
    default this.OnPaint _ = ()

    abstract HitTest : PointF -> bool
    default this.HitTest p = 
        let x = p.X
        let y = p.Y
        (x >= (float32 position.X) || x <= position.X + (float32 size.Width)) && (y >= (float32 position.Y) || y <= position.Y + (float32 size.Height))

    abstract Rotate : float32 -> unit
    default this.Rotate (a : float32) =
        rotateAngle <- rotateAngle + a
        v2wTransform.Rotate(a)
        w2vTransform.Rotate(-a)

    abstract Translate : float32 -> float32 -> unit
    default this.Translate (x : float32)(y : float32) =
        position <- PointF(position.X + x, position.Y + y)
        v2wTransform.Translate(x,y)
        w2vTransform.Translate(-x,-y)
    
    abstract Scale : float32 -> float32 -> unit
    default this.Scale (x : float32)(y : float32) =
        scaleFactor <- PointF(scaleFactor.X * x, scaleFactor.Y * y)
        v2wTransform.Scale(x,y)
        w2vTransform.Scale(1.f/x,1.f/y)
    
    abstract Invalidate : unit -> unit 
    default this.Invalidate() =
        match parent with
        |Some(b) -> b.Invalidate()
        |None -> ()
    
    abstract Id : string with get, set
    default this.Id
        with get() = "LWC"
        and set(v) = ()

    abstract Path : System.Drawing.Drawing2D.GraphicsPath option with get, set
    default this.Path
        with get() = path
        and set(v) = path <- v
    abstract dimTickness : int with get, set
    default this.dimTickness
        with get() = dimThickness
        and set(v) = dimThickness <-v

    member this.W2vTransform
        with get() = w2vTransform
        and set(v) = w2vTransform <- v

    member this.V2wTransform
        with get() = v2wTransform
        and set(v) = v2wTransform <- v
    
    abstract Brush : Brush with get, set             
    default this.Brush
        with get() = brush
        and set(v) = brush <- v

    abstract Interested : bool with get, set
    default this.Interested
        with get() = interested
        and set v = interested <- v

       

    member this.Position
        with get() : PointF = position
        and set(v : PointF) : unit = position <- PointF(0.f,0.f)
                                     this.W2vTransform <- new Drawing2D.Matrix()            //ridefinendo la matrice riparte dai valori di default
                                     this.V2wTransform <- new Drawing2D.Matrix()
                                     let x,y = scaleFactor.X,scaleFactor.Y
                                     let a = rotateAngle
                                     rotateAngle <- 0.f
                                     scaleFactor <- PointF(1.f,1.f)
                                     this.Translate v.X v.Y
                                     this.Rotate a
                                     this.Scale x y
                                     this.Invalidate()
    
    member this.RotateAngle
        with get() = rotateAngle
        and set(v) = rotateAngle <- 0.f
                     this.W2vTransform <- new Drawing2D.Matrix()
                     this.V2wTransform <- new Drawing2D.Matrix()
                     let xP,yP=position.X,position.Y
                     let xS,yS = scaleFactor.X,scaleFactor.Y
                     scaleFactor <- PointF(1.f,1.f)
                     position <- PointF(0.f,0.f)
                     this.Translate xP yP
                     this.Rotate v
                     this.Scale xS yS
                     this.Invalidate()

    member this.ScaleFactor 
        with get() : PointF = scaleFactor
        and set(v : PointF) : unit = scaleFactor <- PointF(1.f,1.f)
                                     this.W2vTransform <- new Drawing2D.Matrix()
                                     this.V2wTransform <- new Drawing2D.Matrix()
                                     let x,y=position.X,position.Y
                                     let a = rotateAngle
                                     rotateAngle <- 0.f
                                     position <- PointF(0.f,0.f)
                                     this.Translate x y
                                     this.Rotate a
                                     this.Scale v.X v.Y
                                     this.Invalidate()

    abstract Size : SizeF with get, set
    default this.Size
        with get() = size
        and set(v) = size <- v; this.Invalidate()

    member this.Parent
        with get() = parent
        and set(v) = parent <- v


type LWContainer(f : Form) as this=
    inherit UserControl()
    let mutable w2v = new Drawing2D.Matrix()
    let mutable v2w = new Drawing2D.Matrix()
    
    let controls = ResizeArray<LWC>()

    do this.Size <- f.ClientSize;this.DoubleBuffered <- true

    let correlate (e : MouseEventArgs) (f:LWC->MouseEventArgs->bool) (outOfHitBox : bool) =
        let mutable found = false
        for i in { (controls.Count - 1) .. -1 ..0} do
            if not found then
                let c = controls.[i]
                let x,y = single e.X,single e.Y
                let v = [|PointF(x,y)|]
                let selected = (e.Button = MouseButtons.Left)
                let pOld = v.[0]
                c.W2vTransform.TransformPoints(v)
                let p = v.[0]
                if (outOfHitBox && c.Interested) || c.HitTest(p) then
                    let evt = new MouseEventArgs(e.Button,e.Clicks,int p.X, int p.Y, e.Delta)
                    found <- f c evt
        found
    
    member this.LWControls = controls

    
    
        
    member this.AddLWC (c : LWPanel) : unit =                
        controls.Add c
        c.LwContainerParent <- Some(this)

    


    override this.OnKeyDown (e:KeyEventArgs) =
        controls |> Seq.iter (fun c ->
            c.OnKeyDown e
        )
        base.OnKeyDown e

    override this.OnMouseDown e =
        correlate e (fun c ev -> c.OnMouseDown(ev)) false |> ignore
        base.OnMouseDown e

    override this.OnMouseUp e =
        correlate e (fun c ev -> c.OnMouseUp(ev)) true |> ignore
        base.OnMouseUp e

    override this.OnMouseMove e =
        correlate e (fun c ev -> c.OnMouseMove(ev)) true |> ignore
        base.OnMouseMove e

    override this.OnPaint e =
        controls |> Seq.iter (fun c ->
           let s = e.Graphics.Save()
           let m = c.V2wTransform
           e.Graphics.MultiplyTransform(m)
           let r = e.Graphics.ClipBounds
           let evt = new PaintEventArgs(e.Graphics, new Rectangle(int(r.Left), int(r.Top), int(r.Width), int(r.Height)) ) 
           c.OnPaint evt
           e.Graphics.Restore(s)
           
           )
        this.Focus() |> ignore
        base.OnPaint(e)


and LWPanel() =
    inherit LWC()

    
    let controls = ResizeArray<LWC>()
    let mutable lwContainerParent : LWContainer option = None
    
    let transformPoint (c : LWC) (e : PointF) : PointF =
        let v = [|e|]
        c.W2vTransform.TransformPoints(v)
        v.[0]

    let correlate (e : MouseEventArgs) (f:LWC->MouseEventArgs->bool) (outOfHitBox : bool) : bool =
        let mutable found = false
        for i in { (controls.Count - 1) .. -1 ..0} do
            if not found then
                let c = controls.[i]
                let x,y = single e.X,single e.Y
                let v = [|PointF(x,y)|]
                c.W2vTransform.TransformPoints(v)
                let p = v.[0]
                if (outOfHitBox && c.Interested) || c.HitTest(p) then
                    let evt = new MouseEventArgs(e.Button,e.Clicks,int p.X, int p.Y, e.Delta)
                    found <- f c evt
        found    
    

    member this.LWControls = controls

    member this.LwContainerParent
        with get() = lwContainerParent
        and set(v) = lwContainerParent <- v
    
    abstract removeLWC : unit -> unit
    default this.removeLWC () : unit =
        let mutable elem = controls.Count
        if(elem > 0) then
            controls.RemoveAt(elem - 1)
        this.Invalidate()

    abstract AddLWC : LWC -> unit
    default this.AddLWC (c: LWC) : unit =
        
        controls.Add c
        c.Parent <- Some(upcast this)
        this.Invalidate()

    override this.HitTest (p : PointF) : bool =
        let mutable res = false
        for c in controls do
            if c.Interested || c.HitTest(transformPoint c p) then res <- true
        res

    override this.Invalidate () =
        match lwContainerParent with
        |None -> base.Invalidate(); 
        |Some(c) -> c.Invalidate()

    override this.OnKeyDown (e:KeyEventArgs) =
        controls |> Seq.iter (fun c ->
            c.OnKeyDown e
        )
        base.OnKeyDown e
     
    override this.OnMouseDown e =
        let x = correlate e (fun c ev -> c.OnMouseDown(ev)) false
        let y = base.OnMouseDown e
        x || y

    override this.OnMouseUp e =
        let x = correlate e (fun c ev -> c.OnMouseUp(ev)) true
        let y = base.OnMouseUp e
        x || y

    override this.OnMouseMove e =
        let x = correlate e (fun c ev -> c.OnMouseMove(ev)) true
        let y = base.OnMouseMove e
        x || y

    override this.OnPaint e =       
        let cont = e.Graphics
        controls |> Seq.iter (fun c ->
           let s = e.Graphics.Save()
           let m = c.V2wTransform
           e.Graphics.MultiplyTransform(m)
           let r = e.Graphics.ClipBounds
           
           let evt = new PaintEventArgs(e.Graphics, new Rectangle(int(r.Left), int(r.Top), int(r.Width), int(r.Height)) ) 
           let g = evt.Graphics
           c.OnPaint evt
           e.Graphics.Restore(s)
           
           )
        base.OnPaint(e)


