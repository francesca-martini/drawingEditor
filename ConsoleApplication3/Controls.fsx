#load "MIDLWC.fsx"
#load "MovingShape.fsx"
#load "IUMShape.fsx"

open IUMShape
open MovingShape
open MIDLWC
open System.Windows.Forms
open System.Drawing

type RectangleColor() =
    inherit LWPanel()


    override this.HitTest(p : PointF) : bool =
        if (p.X >= 0.f && p.X <= this.Size.Width) && (p.Y >= 0.f && p.Y <= this.Size.Height) then
            true            
          else false

    override this.OnPaint e =
         let g = e.Graphics
         g.DrawRectangle(Pens.Black, 0.f,0.f, this.Size.Width,this.Size.Height)
         base.OnPaint e

type MiniRectangleColor() = 
    inherit LWC()    
    let mutable container : RectangleColor option = None     
    
    member this.Container
        with get() = container
        and set(v) = container <- v

    override this.HitTest (p:PointF) : bool =
       if (p.X >= 0.f && p.X <= this.Size.Width) && (p.Y >= 0.f && p.Y <= this.Size.Height) then            
            true            
          else false  

    override this.OnPaint e =
        let g = e.Graphics        
        g.FillRectangle(this.Brush, 0,0, int this.Size.Width,int this.Size.Height)
        g.DrawRectangle(Pens.Black, 0, 0, int this.Size.Width,int this.Size.Height)
        base.OnPaint(e)

    override this.OnMouseDown e =        
        match container with
        |Some(idx) -> idx.Brush <- this.Brush; this.Invalidate()
        |None -> ()
        true      
type IUMTriangle(ref : LWPanel, p : PointF, q : PointF) =
    inherit LWC()
    
    let clickevt = new Event<System.EventArgs>()
    let downevt = new Event<MouseEventArgs>()
    let upevt = new Event<MouseEventArgs>()
    let moveevt = new Event<MouseEventArgs>()

    let pts = [|PointF(); p; q|]
    let mutable path : Drawing2D.GraphicsPath option = None
    let mutable translation : PointF = PointF()

    let bigHitTest (p : PointF) : bool =
        match path with
        |Some(idx) -> 
                      let b = idx.IsVisible(p)
                      b
        |None -> false
    member this.Click = clickevt.Publish
    member this.MouseDown = downevt.Publish
    member this.MouseUp = upevt.Publish
    member this.MouseMove = moveevt.Publish

    member this.Translation 
        with get() = translation
        and set(v) = translation <- v

    override this.HitTest (p: PointF) : bool = 
        bigHitTest(p)
    override this.OnPaint e =
        base.OnPaint e
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        let r = new System.Drawing.Drawing2D.GraphicsPath()
        r.AddPolygon(pts)
        g.FillPath(Brushes.Black,r)
        path <- Some(r)
        g.FillPolygon(Brushes.Black, pts)
    
    override this.OnMouseDown e =
        this.Interested <- true
        base.OnMouseDown e |> ignore
        downevt.Trigger e
        true 

    override this.OnMouseUp e =
        if this.Interested then 
            this.Interested <- false
            upevt.Trigger e
        false


type IUMButton(r : IUMRectanglePanel) as this =
    inherit LWC()

    let mutable refChoice : IUMRectanglePanel option = Some r
    let mutable offset = new PointF()
    let mutable forChoice = 0
    let mutable bip = None
    let mutable dimension = 5

    
    let drawIn (forChoice : int) (g : Graphics) : unit =
        let penna = new Pen(this.Brush, Width = 3.f)        
        match forChoice with
            |1 -> g.DrawEllipse(penna, 10.f, 10.f, this.Size.Width - 20.f,this.Size.Height - 20.f)
            |2 -> g.DrawLine(penna, Point(10, 40), Point(40, 10))
            |3 -> g.DrawBezier(penna, Point(10, 10), Point(10, 40), Point(40, 10), Point(40,40) )
            |4 -> g.DrawRectangle(penna,10.f, 10.f, this.Size.Width - 20.f,this.Size.Height - 20.f)
            |5 -> g.DrawEllipse(penna, 10.f, 10.f, this.Size.Width - 20.f,this.Size.Height - 20.f)
                  g.DrawLine(penna, PointF(this.Size.Width - 7.f,this.Size.Height - 10.f), PointF(this.Size.Width - 7.f,this.Size.Height - 40.f))
            |_ -> ()
    
    abstract Dimension : int with get, set
    default this.Dimension 
        with get() = dimension
        and set(v) = dimension <- v

    member this.ForChoice
        with get() = forChoice
        and set(v) = forChoice <- v
    
    member this.RefChoice
        with get() = refChoice
        and set(v) = refChoice <- v
    
    override this.Brush
        with get() = r.Brush
        and set(v) = ()

    override this.HitTest (p: PointF) : bool =     
          let resx = (p.X - (this.Size.Width/ 2.f)) * (p.X - (this.Size.Width/ 2.f))
          let resy = (p.Y - (this.Size.Height/ 2.f)) * (p.Y - (this.Size.Height/ 2.f))
          resx + resy  <= (this.Size.Width / 2.f) * (this.Size.Width / 2.f)

    override this.OnPaint(e) =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        use b = new SolidBrush(Color.FromArgb(128,0,0,0))
        g.FillEllipse(b, 0.f, 0.f, this.Size.Width,this.Size.Height)
        g.DrawEllipse(Pens.Black,0.f, 0.f, this.Size.Width,this.Size.Height)
        drawIn forChoice g

    override this.OnMouseDown e =
        match this.RefChoice with
        |None -> () 
        |Some(idx) -> idx.Choice <- forChoice
        true

and IUMRectanglePanel(r : RectangleColor) as this =
    inherit LWPanel()

    let mutable bipE = None
    let mutable bipL = None
    let mutable bipC = None
    let mutable bipR = None
    let mutable bipG = None
    let mutable bipEV = None
    let mutable bipLV = None
    let mutable bipCV = None
    let mutable bipRV = None
    let mutable bipGV = None
    
    let mutable ctrlSel = false
    let mutable dimension = 5
    let mutable choice = 0
    let midPanel : LWPanel = new LWPanel()
    let mutable trianglePanel : LWPanel option = None
    let mutable triangles : IUMTriangle[] option = None
    let triangleSide = 20.f
    let movingControls = ResizeArray<IUMMovementEllipse>()
    let mutable context : Graphics option = None
    let mutable scrollDir : int option = None


    let t = new Timer(Interval=30)
    
    let checkCollisions() = ()

    let updateMovement() : unit =
        for b in movingControls do
            b.UpdatePosition()
        checkCollisions()
        this.Invalidate()

    let adjustTriangle (v : SizeF) : unit =
        let p = PointF(triangleSide / 2.f, -triangleSide)
        let q = PointF(triangleSide, 0.f)
        let t0 = new IUMTriangle(midPanel,p,q, 
                                 Position = PointF((v.Width - triangleSide)/2.f,triangleSide/3.f)
                                 )
        t0.MouseDown.Add(fun _ -> scrollDir <- Some 0)
        t0.MouseUp.Add(fun _ -> scrollDir <- None)
        let p = PointF(triangleSide / 2.f, triangleSide)
        let t2 = new IUMTriangle(midPanel,p,q, 
                                 Position = PointF((v.Width - triangleSide)/2.f,v.Height - triangleSide/3.f)
                                 )
        t2.MouseDown.Add(fun _ -> scrollDir <- Some 2)
        t2.MouseUp.Add(fun _ -> scrollDir <- None)
        let p = PointF(triangleSide, triangleSide/2.f)
        let q = PointF(0.f, triangleSide)
        let panel = new LWPanel()
        panel.Parent <- Some (upcast this)

        let t1 = new IUMTriangle(midPanel,p,q, 
                                 Position = PointF(v.Width - triangleSide/3.f, (v.Height - triangleSide)/2.f)
                                 )
        t1.MouseDown.Add(fun _ -> scrollDir <- Some 1)
        t1.MouseUp.Add(fun _ -> scrollDir <- None)
        let p = PointF( -triangleSide, triangleSide/2.f)
        let t3 = new IUMTriangle(midPanel,p,q, 
                                 Position = PointF(triangleSide/3.f, (v.Height - triangleSide)/2.f))
        t3.MouseDown.Add(fun _ -> scrollDir <- Some 3)
        t3.MouseUp.Add(fun _ -> scrollDir <- None)
        let a = [|t0; t1; t2; t3|]
        triangles <- Some a
        trianglePanel <-Some panel
        for t in a do
            panel.AddLWC t

    let passMouseToTriangles (f : LWPanel-> MouseEventArgs -> bool)(e:MouseEventArgs) : bool =
        match trianglePanel with
        |None -> false
        |Some p -> f p e  
    
    
    let intersecateSimple (p : PointF) (s : SizeF) (q : PointF) (t : SizeF) : (PointF * SizeF) option =
        let cd = p.X + s.Width
        let rs = q.X
        let cs = p.X
        let rd = q.X + t.Width
        let ca = p.Y
        let cb = p.Y + s.Height
        let ra = q.Y
        let rb = q.Y + t.Height
        let mutable delta = cd - rs
        let mutable newLoc = PointF(rs - cd, 0.f)
        let mutable newSpeed = SizeF(-1.f,1.f)
        if (rd - cs) < delta then delta <- rd - cs
                                  newLoc <- PointF(delta,0.f)
        if (rb - ca) < delta then delta <- rb - ca
                                  newLoc <- PointF(0.f, delta)
                                  newSpeed <- SizeF(1.f,-1.f)
        if (cb - ra) < delta then delta <- cb - ra
                                  newLoc <- PointF(0.f,-delta)
                                  newSpeed <- SizeF(1.f,-1.f)
        if (delta < 0.f) then None
        else Some(newLoc,newSpeed)


    let collide (b : LWC) (c : LWC) : unit =
       if b :? IUMMovementEllipse && (c :? IUMRectangle || c :? IUMEllipse) then
            let res = (intersecateSimple b.Position b.Size c.Position c.Size)
            match res with 
            |None -> ()
            |Some (loc,speed) -> let b = b :?> IUMMovementEllipse 
                                 b.Position <- PointF(b.Position.X + loc.X, b.Position.Y + loc.Y)
                                 b.Speed <- SizeF(b.Speed.Width * speed.Width, b.Speed.Height * speed.Height)
            
        

    let handleCollisions () : unit =
        for a in midPanel.LWControls do
            for b in midPanel.LWControls do
              if a <> b then collide a b
            
 
    let handleScroll () : unit =
        match scrollDir with
        |None ->()
        |Some 0 -> midPanel.Translate 0.f 10.f
        |Some 1 -> midPanel.Translate -10.f 0.f
        |Some 2 -> midPanel.Translate 0.f -10.f
        |Some 3 -> midPanel.Translate 10.f 0.f
        |_ -> ()

    do 
       this.Brush <- r.Brush
       base.AddLWC (midPanel)
       t.Tick.Add(fun _ ->
                            updateMovement()
                            handleCollisions() 
                            handleScroll()
                            this.Invalidate()
       ); 
       t.Start()
       this.Interested <- true

    member this.StopTimer () : unit = 
        t.Stop()

    abstract Dimension : int with get, set
    default this.Dimension 
        with get() = dimension
        and set(v) = dimension <- v
    
    member this.Choice
        with get() = choice
        and set(v) = choice <- v   

    override this.Size
        with get() = base.Size
        and set (v) = base.Size <- v
                      adjustTriangle (v)
    override this.Brush 
        with get() = r.Brush
        and set(v) = ()
    
    override this.AddLWC(l : LWC) : unit =
        midPanel.AddLWC(l)
        l.Parent <- Some (upcast midPanel)

    member this.AddMovementEllipse(c : IUMMovementEllipse) : unit = 
        movingControls.Add c
        this.AddLWC c
    
    override this.removeLWC() : unit =
        midPanel.removeLWC()
        



    override this.HitTest (p: PointF) : bool =     
          if (p.X >= 0.f && p.X <= this.Size.Width) && (p.Y >= 0.f && p.Y <= this.Size.Height) then
            true
          else match trianglePanel with
                |None -> base.HitTest p
                |Some idx ->  idx.HitTest p 
                   
    override this.OnPaint(e) =
        let g = e.Graphics
        context <- Some g
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.FillRectangle(Brushes.White, 0.f, 0.f, this.Size.Width,this.Size.Height)
        g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Size.Width - 1.f,this.Size.Height - 1.f)

        match trianglePanel with
        | None -> ()
        | Some p -> p.OnPaint e
        
        e.Graphics.Clip <- new Region(RectangleF(1.f, 1.f, this.Size.Width - 2.f, this.Size.Height - 2.f))
        let r = e.Graphics.ClipBounds  
        let evt = new PaintEventArgs(e.Graphics, new Rectangle(int(r.Left), int(r.Top), int(r.Width), int(r.Height)) ) 
        let g = evt.Graphics

        
        base.OnPaint e

    override this.OnMouseDown e =
        let res = passMouseToTriangles (fun p -> p.OnMouseDown) e
        let p = PointF(single e.Location.X, single e.Location.Y)
        if this.HitTest p then
            if not res && not (base.OnMouseDown e) then
                let p = new PointF(single e.Location.X,single e.Location.Y)

                let c = this.Choice
                match e.Button with
                |MouseButtons.Left ->
                        let v = [|p|]
                        midPanel.W2vTransform.TransformPoints v
                        let p = v.[0]
                        match c with
                        |1 -> let el = new IUMEllipse()
                              el.dimTickness <- dimension
                              let elMove = new IUMEllipseMove()
                              elMove.dimTickness <- dimension
                              el.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Size <- SizeF(0.f,0.f)
                              el.Brush <- this.Brush
                              elMove.Brush <- this.Brush
                              bipE <- Some(el)
                              bipEV <- Some(elMove)
                              this.AddLWC(elMove)
                        |2 -> let el = new IUMLine()
                              el.dimTickness <- dimension
                              let elMove = new IUMLineMove()
                              elMove.dimTickness <- dimension
                              elMove.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Size <- SizeF(0.f,0.f)
                              elMove.Brush <- this.Brush
                              el.Position <- PointF(single(p.X), single(p.Y))
                              el.Brush <- this.Brush
                              bipL <- Some(el)
                              bipLV <- Some(elMove)
                              this.AddLWC(elMove)
                        |3 -> let el = new IUMCurve()
                              el.dimTickness <- dimension
                              el.Position <- PointF(single(p.X), single(p.Y))
                              el.Brush <- this.Brush
                              this.AddLWC(el)
                              bipC <- Some(el)
                        |4 -> let el = new IUMRectangle()
                              el.dimTickness <- dimension
                              let elMove = new IUMRectangleMove()
                              elMove.dimTickness <- dimension
                              el.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Size <- SizeF(0.f,0.f)
                              el.Brush <- this.Brush
                              elMove.Brush <- this.Brush
                              bipR <- Some(el)
                              bipRV <- Some(elMove)
                              this.AddLWC(elMove)
                        |5 -> let el = new IUMMovementEllipse(midPanel.LWControls)
                              el.dimTickness <- dimension
                              let elMove = new IUMMovementEllipseMove()
                              elMove.dimTickness <- dimension
                              el.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Position <- PointF(single(p.X), single(p.Y))
                              elMove.Size <- SizeF(0.f,0.f)
                              el.Brush <- this.Brush
                              elMove.Brush <- this.Brush
                              bipG <- Some(el)
                              bipGV <- Some(elMove)
                              this.AddLWC(elMove)
                        |_ -> ()
                |_ ->()                    
        true

    override this.OnMouseUp e =
        let res = passMouseToTriangles (fun p -> p.OnMouseUp) e
        let p = PointF(single e.Location.X, single e.Location.Y)
        let v = [|p|]
        midPanel.W2vTransform.TransformPoints v
        let p = v.[0]
        base.OnMouseUp(e) |> ignore
        match bipE with
            | Some b -> b.Size <- SizeF(float32 p.X - b.Position.X,float32 p.Y - b.Position.Y)
                        this.removeLWC()
                        this.AddLWC(b)                        
                        this.Invalidate()     
                        bipEV <- None
                        bipE <- None              
            | None -> ()
        match bipG with
            | Some b -> b.Size <- SizeF(float32 p.X - b.Position.X , float32 (sign (p.Y - b.Position.Y)) * abs(float32 p.X - b.Position.X))
                        this.removeLWC()
                        this.AddMovementEllipse(b)                        
                        this.Invalidate()     
                        bipGV <- None
                        bipG <- None              
            | None -> ()

        match bipC with
            | Some b -> b.Size <- SizeF(float32 p.X - b.Position.X,float32 p.Y - b.Position.Y)
                        this.AddLWC(b)
                        this.Invalidate()
                        bipCV <- None     
                        bipC <- None
            | None -> ()

        match bipL with
            | Some b -> b.Size <- SizeF(float32 p.X - b.Position.X,float32 p.Y - b.Position.Y)
                        this.removeLWC()
                        this.AddLWC(b)
                        this.Invalidate()
                        bipLV <- None
                        bipL <- None              
            | None -> ()            

        match bipR with
            | Some b -> b.Size <- SizeF(float32 p.X - b.Position.X,float32 p.Y - b.Position.Y)
                        this.removeLWC()
                        this.AddLWC(b)
                        this.Invalidate() 
                        bipRV <- None    
                        bipR <- None              
            | None -> ()
          
        false


    override this.OnMouseMove e =
        let p = PointF(single e.Location.X, single e.Location.Y)
        let v = [|p|]
        midPanel.W2vTransform.TransformPoints v
        let p = v.[0]

        passMouseToTriangles (fun p -> p.OnMouseMove) e |> ignore
        base.OnMouseMove(e) |> ignore
        let f (b : LWC) = b.Size <- SizeF(float32 p.X - b.Position.X,float32 p.Y - b.Position.Y)
        match bipLV,bipEV,bipRV,bipGV with
            | Some b,None,None,None -> f b 
            | None,Some b,None,None -> f b
            | None,None,Some b,None -> f b
            | None,None,None,Some b -> f b
            | _,_,_,_ -> ()
        this.Invalidate()

        false
    
    override this.OnKeyDown e =
        match e.KeyCode with
         |Keys.ControlKey -> ctrlSel <- true
         |Keys.Z -> if ctrlSel then this.removeLWC(); this.Invalidate()
         |_ -> ()

    override this.OnKeyUp e =
        
        match e.KeyCode with
         |Keys.ControlKey -> ctrlSel <- false
         |_ -> ()

type Text() =
    inherit LWC()

    override this.OnPaint (e) =
        let g = e.Graphics
        let f = new Font("Arial", 15.f)
        let sz = g.MeasureString("Draw inside this rectangle",f)
        g.DrawString("Draw inside this rectangle", f, Brushes.Blue, PointF(0.f,0.f))
    
    override this.HitTest e = false




    
