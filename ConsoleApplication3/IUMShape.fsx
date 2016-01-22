#load "MIDLWC.fsx" 
#load "MovingShape.fsx"


open MIDLWC
open System.Windows.Forms
open System.Drawing
open MovingShape
type IUMMovementEllipse(r : ResizeArray<LWC> ) =
    inherit LWC()
    
    let handleSize = 10.f    
    let mutable offsetDrag = PointF()
    let mutable first = true
    let mutable activateMan = false
    let mutable selected = None
    let mutable initialPoint : PointF option = None
    let mutable finalPoint : PointF option = None
    let mutable bipLV : IUMLineMove option = None

    let mutable speed = SizeF(0.f,0.f) 
    let mutable lastT = System.DateTime.Now
    


    let handleHitTest (p:PointF) (h:PointF) (r : float32) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < r * r

    override this.Size
        with get () = base.Size
        and set(v) = let xl,xs = if v.Width < 0.f then 
                                            this.Position.X + v.Width, -v.Width
                                            else  this.Position.X , v.Width
                     let yl,ys = if v.Height < 0.f then 
                                            this.Position.Y + v.Height, -v.Height
                                            else this.Position.Y , v.Height
                     this.Position <- PointF(xl,yl)
                     base.Size <- SizeF(xs,ys)

    member this.Speed with get() = speed and set(v) = speed <- v
    member this.CenterLocation with get() = PointF(this.Position.X + this.Size.Width / 2.f, this.Position.Y + this.Size.Height / 2.f)

    member this.UpdatePosition() =
        let t = System.DateTime.Now
        let dt = t - lastT
        let vx = speed.Width / 1000.f
        let vy = speed.Height / 1000.f
        let dx = vx * single(dt.TotalMilliseconds)
        let dy = vy * single(dt.TotalMilliseconds)
        this.Position <- PointF(this.Position.X + dx, this.Position.Y + dy)
        lastT <- t

    member this.Selected
        with get () = selected
        and set v = selected <- v
    
    override this.HitTest (p: PointF) : bool =                
        let w = this.Size.Width / 2.f
        handleHitTest p (PointF(w,w)) w

          
    override this.OnPaint(e) =
        base.OnPaint(e)
        let g = e.Graphics
        

        match initialPoint, finalPoint with
        |Some idx1, Some idx2 -> let v = [|idx1;idx2|]
                                 g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
                                 let w = this.Size.Width/2.f
                                 let tx, ty = idx2.X - idx1.X + w, idx2.Y - idx1.Y + w
                                 use pen = new Pen(Brushes.Black,2.f)
                                 pen.EndCap <- Drawing2D.LineCap.ArrowAnchor

                                 g.DrawLine(pen, PointF(this.Size.Width/2.f,this.Size.Width/2.f), PointF(tx,ty))

        |_,_ -> ()

        let drawHandle (p:PointF) =
          let w = 10.f
          g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)       

        let ret = new Rectangle(0 ,0,int this.Size.Width, int this.Size.Height)
        let c = new System.Drawing.Drawing2D.GraphicsPath()
        c.AddEllipse(ret)
        this.Path <- Some(c)
        g.FillPath(this.Brush,c)
    override this.OnMouseDown e =
        match e.Button with
        |MouseButtons.Right -> 
                               this.Interested <- true
                               let point = PointF(single e.Location.X,single e.Location.Y)
                               let v = [|point|]
                               this.V2wTransform.TransformPoints v
                               initialPoint <- Some(v.[0])
                               finalPoint <- initialPoint

        |_ -> ()
        true

    override this.OnMouseUp e = 
        match e.Button with
        |MouseButtons.Right -> this.Interested <- false
                               let point = PointF(single e.Location.X,single e.Location.Y)
                               let v = [|point|]
                               this.V2wTransform.TransformPoints v
                               
                               match initialPoint with
                                |None -> ()
                                |Some idx -> let p = SizeF(v.[0].X - idx.X ,v.[0].Y - idx.Y)
                                             this.Speed <- p
                               initialPoint <- None
                               finalPoint <- None
        |_ -> ()
        false
    override this.OnMouseMove e =
      match e.Button with
        |MouseButtons.Right -> let point = PointF(single e.Location.X,single e.Location.Y)
                               let v = [|point|]
                               this.V2wTransform.TransformPoints v
                               match initialPoint with
                                |None -> ()
                                |Some idx -> 
                                             finalPoint <- Some v.[0]
                                
        |_ -> ()
      false
type IUMEllipse() as this =
    inherit LWC()
    
    let mutable pts = [| PointF(); PointF(); PointF(); PointF() |]
    let handleSize = 10.f
    let mutable selected = None
    let mutable offsetDrag = PointF()
    let mutable first = true
    let mutable activateMan = false

    
    
    let bigHitTest (p : PointF) : bool =
        match this.Path with
        |Some(idx) -> idx.IsVisible(p)
        |None -> false

    let handleHitTest (p:PointF) (h:PointF) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < handleSize * handleSize
    
    override this.Size 
        with get () = base.Size
        and set(v) = let xl,xs = if v.Width < 0.f then
                                                    match  this.Selected with
                                                    | Some i -> if i = 1 then this.Selected <- Some 3
                                                                else if i = 3 then this.Selected <- Some 1
                                                    | None -> () 
                                                    this.Position.X + v.Width, -v.Width
                                            else  this.Position.X , v.Width
                     let yl,ys = if v.Height < 0.f then 
                                                    match  this.Selected with
                                                    | Some i -> if i = 0 then this.Selected <- Some 2
                                                                else if i = 2 then this.Selected <- Some 0
                                                    | None -> () 
                                                    this.Position.Y + v.Height, -v.Height
                                            else  this.Position.Y , v.Height
                     this.Position <- PointF(xl,yl)
                     base.Size <- SizeF(xs,ys)
                     pts <- [| PointF(this.Size.Width/2.f,0.f); PointF(this.Size.Width, this.Size.Height/2.f); PointF(this.Size.Width/2.f, this.Size.Height); PointF(0.f, this.Size.Height/2.f) |]
    override this.Interested
        with get () = selected.IsSome
        and set(v) = ()

    member this.Selected
        with get () = selected
        and set v = selected <- v
    
    override this.HitTest (p: PointF) : bool =                
            let ht = handleHitTest p
            match pts |> Array.tryFindIndex ht with
            | Some _ -> true
            | None -> bigHitTest(p)
        
             

    
    override this.OnMouseDown e =
        base.OnMouseDown e |> ignore
        let l = PointF(single e.Location.X,single e.Location.Y)
        if activateMan then
            let ht = handleHitTest l
            this.Selected <- pts |> Array.tryFindIndex ht

        match e.Button with 
        |MouseButtons.Right -> match selected with
                               |Some(idx)->()
                               |None -> 
                                        if activateMan then activateMan <- false 
                                        else activateMan <- bigHitTest(PointF(single e.Location.X,single e.Location.Y))
                                             
                                        this.Invalidate()
                                        
        |MouseButtons.Left -> 
                               match this.Selected with
                               |Some(idx) -> 
                                             let p = pts.[idx]
                                             offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
                                             this.Interested <- true
                               |None -> ()
        | _ -> ()
        true

    override this.OnMouseUp e =
        this.Selected <- None
        
        this.Interested <- false
        false

    override this.OnMouseMove e =
        let l = PointF(single e.Location.X,single e.Location.Y)
        
        match this.Selected with
        | Some idx -> 
            match idx with
            | 1 -> pts.[idx] <- PointF(l.X + offsetDrag.X, pts.[idx].Y)
                   this.Size <- SizeF(pts.[1].X - pts.[3].X, this.Size.Height)
                   pts.[0] <- PointF(this.Size.Width/2.f, 0.f)
                   pts.[2] <- PointF(this.Size.Width/2.f, this.Size.Height)
                   pts.[3] <- PointF(0.f, this.Size.Height/2.f)

            | 3 -> let old = pts.[idx].X
                   this.Size <- SizeF(pts.[1].X - l.X - offsetDrag.X, this.Size.Height)
                   this.Translate (l.X + offsetDrag.X) 0.f
                   pts.[0] <- PointF(this.Size.Width/2.f, 0.f)
                   pts.[2] <- PointF(this.Size.Width/2.f, this.Size.Height)
                   pts.[1] <- PointF(this.Size.Width, this.Size.Height/2.f)                   
            | 2 -> pts.[idx] <- PointF(pts.[idx].X, l.Y + offsetDrag.Y)
                   this.Size <- SizeF(this.Size.Width, pts.[2].Y - pts.[0].Y)
                   pts.[3] <- PointF(0.f, this.Size.Height/2.f)
                   pts.[1] <- PointF(this.Size.Width, this.Size.Height/2.f)                   
            | 0 -> 
                   this.Size <- SizeF(this.Size.Width, pts.[2].Y - l.Y - offsetDrag.Y)
                   this.Translate 0.f (l.Y + offsetDrag.Y)
                   pts.[2] <- PointF(this.Size.Width/2.f, this.Size.Height)
                   pts.[3] <- PointF(0.f, this.Size.Height/2.f)
                   pts.[1] <- PointF(this.Size.Width, this.Size.Height/2.f)                   
            | _ -> ()
            this.Invalidate()
        | None -> ()
        false
    
          
    override this.OnPaint(e) =
        base.OnPaint(e)
        let g = e.Graphics
        let drawHandle (p:PointF) =
          let w = 10.f
          g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        

        if first then
            pts.[0] <- PointF(this.Size.Width/2.f,0.f)
            pts.[1] <- PointF(this.Size.Width, this.Size.Height/2.f)
            pts.[2] <- PointF(this.Size.Width/2.f, this.Size.Height)
            pts.[3] <- PointF(0.f, this.Size.Height/2.f)
            first <- false
        if activateMan then
            pts |> Array.iter drawHandle 
        let ret = new Rectangle(0 ,0,int( pts.[1].X - pts.[3].X), int (pts.[2].Y - pts.[0].Y))
        let r = new System.Drawing.Drawing2D.GraphicsPath()
        r.AddEllipse(ret)
        this.Path <- Some(r)
        g.FillPath(this.Brush,r)
          


type IUMRectangle() as this =
    inherit LWC()
        
    let mutable pts = [| PointF(0.f,0.f); PointF(0.f,0.f); PointF(0.f,0.f); PointF(0.f,0.f) |]
    let handleSize = 10.f
    let mutable selected = None
    let mutable offsetDrag = PointF()
    let mutable activateMan = false
    
    let bigHitTest (p : PointF) : bool =
        if (p.X >= 0.f && p.X <= this.Size.Width) && (p.Y >= 0.f && p.Y <= this.Size.Height) then
            true            
          else false
    
    let handleHitTest (p:PointF) (h:PointF) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < handleSize * handleSize

    override this.Size 
        with get () = base.Size
        and set(v) =
                     let wn, hn = v.Width < 0.f, v.Height < 0.f 
                     let xl,xs = if wn then this.Position.X + v.Width, -v.Width
                                            else  this.Position.X , v.Width
                     let yl,ys = if hn then this.Position.Y + v.Height, -v.Height
                                            else  this.Position.Y , v.Height
                     if wn then
                        match selected with
                        | Some 0 -> selected <- Some 1
                        | Some 1 -> selected <- Some 0
                        | Some 2 -> selected <- Some 3
                        | Some 3 -> selected <- Some 2
                        | _ -> ()

                     if hn then
                        match selected with
                        | Some 0 -> selected <- Some 2
                        | Some 1 -> selected <- Some 3
                        | Some 2 -> selected <- Some 0
                        | Some 3 -> selected <- Some 1
                        | _ -> ()
                     this.Position <- PointF(xl,yl)
                     base.Size <- SizeF(xs,ys)
                     pts <- [| PointF(0.f,0.f); PointF(this.Size.Width, 0.f); PointF(0.f, this.Size.Height); PointF(this.Size.Width, this.Size.Height) |]

    override this.HitTest (p: PointF) : bool =     
        
        let ht = handleHitTest p
        match pts |> Array.tryFindIndex ht with
         | Some _ -> true
         | None -> bigHitTest(p)
    




    override this.OnMouseDown e =
        base.OnMouseDown e |> ignore
        let l = PointF(single e.Location.X,single e.Location.Y)
        if activateMan then
            let ht = handleHitTest l
            selected <- pts |> Array.tryFindIndex ht

        match e.Button with
        |MouseButtons.Right -> match selected with
                                |Some(idx)->()
                                |None -> 
                                         if activateMan then activateMan <- false 
                                         else activateMan <- bigHitTest(PointF(single e.Location.X,single e.Location.Y))
                                              
                                         this.Invalidate()
                                        
        |MouseButtons.Left -> 
                                match selected with
                                |Some(idx) -> 
                                                let p = pts.[idx]
                                                offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
                                                this.Interested <- true
                                |None -> ()
        |_ -> ()
        true


    override this.OnMouseUp e =
        selected <- None
        this.Interested <- false
        false

    override this.OnMouseMove e =
        let l = PointF(single e.Location.X,single e.Location.Y)
        
        match selected with
        | Some idx -> 
            match idx with
            | 1 -> pts.[idx] <- PointF(l.X + offsetDrag.X, 0.f)
                   this.Size <- SizeF(pts.[1].X - pts.[0].X, this.Size.Height - l.Y - offsetDrag.Y)
                   this.Translate(0.f) (l.Y + offsetDrag.Y)
                   pts.[2] <- PointF(0.f, this.Size.Height)
                   pts.[3] <- PointF(this.Size.Width, this.Size.Height)

            | 3 -> let old = pts.[idx].X
                   pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
                   this.Size <- SizeF(pts.[3].X - pts.[2].X, pts.[3].Y - pts.[1].Y)
                   pts.[2] <- PointF(pts.[2].X, this.Size.Height)
                   pts.[1] <- PointF(this.Size.Width, pts.[1].Y)                   
            | 2 -> this.Size <- SizeF(this.Size.Width - l.X - offsetDrag.X,l.Y + offsetDrag.Y)
                   this.Translate (l.X + offsetDrag.X) (0.f)
                   pts.[3] <- PointF(this.Size.Width, this.Size.Height)
                   pts.[1] <- PointF(this.Size.Width, 0.f)                   
            | 0 -> 
                   this.Size <- SizeF(this.Size.Width - l.X - offsetDrag.X, this.Size.Height - l.Y - offsetDrag.Y)
                   this.Translate(l.X + offsetDrag.X) (l.Y  + offsetDrag.Y)
                   pts.[2] <- PointF(0.f, this.Size.Height)
                   pts.[3] <- PointF(this.Size.Width, this.Size.Height)
                   pts.[1] <- PointF(this.Size.Width, 0.f)                   
            | _ -> ()
            this.Invalidate()
        | None -> ()
        false
    
    
              
    override this.OnPaint(e) =
        base.OnPaint(e) 
        let g = e.Graphics
        let drawHandle (p:PointF) =
          let w = 10.f
          g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        

        pts.[0] <- PointF(0.f,0.f)
        pts.[1] <- PointF(this.Size.Width, 0.f)
        pts.[2] <- PointF(0.f, this.Size.Height)
        pts.[3] <- PointF(this.Size.Width, this.Size.Height)
        if activateMan then 
            pts |> Array.iter drawHandle          
        let w,h = this.Size.Width, this.Size.Height
        let x, w = if w < 0.f then w, -w else 0.f, w
        let y, h = if h < 0.f then h, -h else 0.f, h

        g.FillRectangle(this.Brush, x, y, w, h)

type IUMLine() as this =
    inherit LWC()

    let pts = [| PointF(0.f,0.f); PointF(this.Size.Width, this.Size.Height) |]
    let handleSize = 10.f
    let mutable selected = None
    let mutable offsetDrag = PointF()
    
    let mutable activateMan = false

    do this.Size <- new SizeF(160.f,50.f)
       this.Position <- new PointF(10.f,10.f)       
    
    let calcM (p : PointF) (q : PointF) : float32 option =
        if(p.X = q.X) then None
        else Some ((q.Y - p.Y) / (q.X - p.X))

    let calcQ (p : PointF) (m : float32) : float32 =
        p.Y - (m * p.X)
    
    let checkLarge (thickness : float32) (a : PointF) (b : PointF) (c : PointF) : bool =      //dice se il terzo punto sta fra i primi due in larghezza
        match calcM a b with
        |None -> 
                 let a, b = if a.Y <= b.Y then a, b else b, a
                 c.Y >= a.Y && c.Y <= b.Y && c.X >= a.X - thickness && c.X <= a.X + thickness
        |Some m when m <> 0.f -> let q = calcQ a m
                                 let q1 = calcQ b -m
                                 let q0 = calcQ a -m
                                 let qf = calcQ c -m
                                 let xi = (qf - q) / (2.f * m)
                                 let yi = (qf + q) / 2.f
                                 let x,y = c.X, c.Y                   
                                 let distance = sqrt ((xi - x) * (xi - x) + (yi - y) * (yi - y))
                                 ((y >= (-m * x) + q0 && y <= (-m * x) + q1 ) || 
                                  (y <= (-m * x) + q0 && y >= (-m * x) + q1 ) ) &&
                                  (distance <= thickness)
        |Some m -> let a, b = if a.X <= b.X then a, b else b, a
                   c.X >= a.X && c.X <= b.X && c.Y >= a.Y - thickness && c.Y <= a.Y + thickness



    
    
    let bigHitTest (p : PointF) : bool =
        let r = checkLarge 5.f pts.[0] pts.[1] p
        r 

    let handleHitTest (p:PointF) (h:PointF) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < handleSize * handleSize

    member this.Selected
        with get () = selected
        and set v = selected <- v
    
    override this.HitTest (p: PointF) : bool =                
            let ht = handleHitTest p
            match pts |> Array.tryFindIndex ht with
            | Some _ -> true
            | None -> bigHitTest(p)
        


            
    override this.OnMouseDown e =
        base.OnMouseDown e |> ignore
        let l = PointF(single e.Location.X,single e.Location.Y)
        if activateMan then
            let ht = handleHitTest l
            this.Selected <- pts |> Array.tryFindIndex ht

        match e.Button with
        |MouseButtons.Right -> match selected with
                               |Some(idx)->()
                               |None -> 
                                        if activateMan then activateMan <- false 
                                        else activateMan <- bigHitTest(PointF(single e.Location.X,single e.Location.Y))
                                             
                                        this.Invalidate()
                                        
        |MouseButtons.Left -> 
                               match this.Selected with
                               |Some(idx) -> 
                                             let p = pts.[idx]
                                             offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
                                             this.Interested <- true
                               |None -> ()
        |_ -> ()
        true

    override this.OnMouseUp e =
        this.Selected <- None
        
        this.Interested <- false
        false

    override this.OnMouseMove e =
        let l = PointF(single e.Location.X,single e.Location.Y)
        
        match this.Selected with
        | Some idx -> 
            match idx with
            | 0 -> 
                   this.Translate (l.X + offsetDrag.X) (l.Y + offsetDrag.Y)
                   this.Size <- SizeF(this.Size.Width - l.X - offsetDrag.X, this.Size.Height - l.Y - offsetDrag.Y)
                   pts.[1] <- PointF(this.Size.Width, this.Size.Height)
                   
            | 1 -> pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
                   this.Size <- SizeF(pts.[0].X + l.X + offsetDrag.X, pts.[0].Y + l.Y + offsetDrag.Y)
                                      
            | _ -> ()

            this.Invalidate()
        | None -> ()
        false
    
          
    override this.OnPaint(e) =
        let g = e.Graphics
        let drawHandle (p:PointF) =
          let w = 10.f
          g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        let pen = new Pen(this.Brush,float32 this.dimTickness)
        g.DrawLine(pen, PointF(0.f,0.f), PointF(this.Size.Width,this.Size.Height))   
        pts.[0] <- PointF(0.f,0.f)
        pts.[1] <- PointF(this.Size.Width, this.Size.Height)
        if activateMan then
            pts |> Array.iter drawHandle
        base.OnPaint(e) 

type IUMCurve() as this =
    inherit LWC()
    
    let pts = [| PointF(); PointF(); PointF(); PointF() |]
    let handleSize = 10.f
    let mutable selected = None
    let mutable offsetDrag = PointF()
    let mutable activateMan = false
    let pen = new Pen(this.Brush,float32 this.dimTickness)
    
    let bigHitTest (p : PointF) : bool =
        match this.Path with
        |Some(idx) -> 
                      let b = idx.IsOutlineVisible(p,pen)
                      b
        |None -> false

    let handleHitTest (p:PointF) (h:PointF) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < handleSize * handleSize

    override this.Size
        with get() = base.Size
        and set(v) = base.Size <- v
                     pts.[0] <- PointF(0.f,0.f)
                     pts.[1] <- PointF(v.Width, 0.f)
                     pts.[2] <- PointF(0.f, v.Height)
                     pts.[3] <- PointF(v.Width, v.Height)

    override this.HitTest (p: PointF) : bool =                
            let ht = handleHitTest p
            match pts |> Array.tryFindIndex ht with
            | Some _ -> true
            | None -> bigHitTest(p)
                           
    override this.OnMouseDown e =
        base.OnMouseDown e |> ignore
        let l = PointF(single e.Location.X,single e.Location.Y)
        
        if activateMan then  
            let ht = handleHitTest l          
            selected <- pts |> Array.tryFindIndex ht

        match e.Button with 
        |MouseButtons.Right -> match selected with
                               |Some(idx)->()
                               |None -> 
                                        if activateMan then activateMan <- false 
                                        else activateMan <- bigHitTest(PointF(single e.Location.X,single e.Location.Y))
                                            
                                        this.Invalidate()
                                        
        |MouseButtons.Left -> 
                               match selected with
                               |Some(idx) -> 
                                             let p = pts.[idx]
                                             offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
                                             this.Interested <- true
                               |None -> ()
        | _ -> ()
        true
    


    override this.OnMouseMove e =
        let l = PointF(single e.Location.X,single e.Location.Y)
        
        match selected with
        | Some idx -> 
            match idx with
            | 1 -> pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
            | 3 -> pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
            | 2 -> pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
            | 0 ->  let x,y = (l.X + offsetDrag.X),(l.Y + offsetDrag.Y)
                    this.Translate x y
                    pts.[2] <- PointF(pts.[2].X - x, pts.[2].Y - y)
                    pts.[3] <- PointF(pts.[3].X - x, pts.[3].Y - y)
                    pts.[1] <- PointF(pts.[1].X - x, pts.[1].Y - y)
            | _ -> ()
            this.Invalidate()
        | None -> ()
        false



    override this.OnMouseUp e =
        selected <- None        
        this.Interested <- false
        false

    


                  
    override this.OnPaint(e) =
        let g = e.Graphics
        let g = e.Graphics
        let drawHandle (p:PointF) =
          let w = 10.f
          g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        let pen = new Pen(this.Brush,float32 this.dimTickness)
        
        if activateMan then
            pts |> Array.iter drawHandle  
        let r = new System.Drawing.Drawing2D.GraphicsPath()
        r.AddBezier(pts.[0],pts.[1],pts.[2],pts.[3])
        g.DrawPath(pen,r)
        this.Path <- Some(r)
        base.OnPaint(e) 
