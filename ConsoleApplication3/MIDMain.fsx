#load "MIDLWC.fsx"
#load "Controls.fsx"
(* 
 * NOTE:
 * Per ottenere le maniglie con le quali si modella il controllo cliccare su di esso con il click destro del mouse
 * il 4 cerchio implimenta le palline in movimento. Per cambiare la loro velocità ad esse dopo averle create trascinare il click destro
 * del mouse da esse ad un punto qualsiasi. La pallina parte da una velocità iniziale 0,0. 
 * Per annullare l'inserimento di una qualsiasi forma premere CTRL + z.
 *
 *)
open MIDLWC
open Controls
open System.Windows.Forms
open System.Drawing

let addC (c : ResizeArray<Brush>) : unit =
    c.Add(Brushes.Black)
    c.Add(Brushes.White)
    c.Add(Brushes.Red)
    c.Add(Brushes.Magenta)
    c.Add(Brushes.Pink)
    c.Add(Brushes.Lime)
    c.Add(Brushes.Violet)
    c.Add(Brushes.Cyan)
    c.Add(Brushes.Gold)
    c.Add(Brushes.LightBlue)
    c.Add(Brushes.LightSkyBlue)
    c.Add(Brushes.Blue)
    c.Add(Brushes.Green)
    c.Add(Brushes.Yellow)    
    c.Add(Brushes.Gray)    
    c.Add(Brushes.Coral)
    c.Add(Brushes.DarkKhaki)
    c.Add(Brushes.Chocolate)
    c.Add(Brushes.Brown)   
    c.Add(Brushes.Silver)
    
    
    
    

let f = new Form(Text = "Drawing editor", TopMost = true, Size = Size(600,600))

let mutable dim = "5"
let tb = new TextBox(Text = "5", Dock = DockStyle.Bottom)
let lwp = new LWContainer(f, Size = Size(600,520))
f.Controls.Add(lwp)
let lwc = new LWPanel(Position = PointF(10.f,10.f), Size = SizeF(500.f,500.f))

lwp.AddLWC(lwc)

let buttons = ResizeArray<IUMButton>()
let txt = new Text(Position = PointF(10.f,60.f),Size = SizeF(200.f,20.f))

let colorRet = new RectangleColor(Position = PointF(360.f, 10.f), Size = SizeF(10.f * 20.f, 40.f))
let mutable indexx,indexy = 0,0

let c = new ResizeArray<Brush>() 
addC c
for i in c  do
    colorRet.AddLWC(new MiniRectangleColor(Container = Some(colorRet), Brush = i, Position = PointF(single indexx,single indexy), Size = SizeF(20.f,20.f)))
    indexx <- indexx + 20
    if indexx >= 10 * 20 then
        indexx <- 0
        indexy <- indexy + 20

let drawRect = new IUMRectanglePanel(colorRet, Brush = Brushes.Black, Size = SizeF(single lwp.Size.Width - 50.f, single lwp.Size.Height - 130.f), Position=PointF(5.f,100.f))

f.Controls.Add(tb)

lwc.AddLWC colorRet
lwc.AddLWC txt
lwc.AddLWC drawRect

buttons.Add(new IUMButton(drawRect, Brush = Brushes.Black, Size = SizeF(50.f,50.f), ForChoice = 1))
buttons.Add(new IUMButton(drawRect,Brush = Brushes.Black, Size = SizeF(50.f,50.f), Position=PointF(70.f,0.f), ForChoice = 2))
buttons.Add(new IUMButton(drawRect,Brush = Brushes.Black, Size = SizeF(50.f,50.f), Position=PointF(140.f,0.f), ForChoice = 3))
buttons.Add(new IUMButton(drawRect,Brush = Brushes.Black, Size = SizeF(50.f,50.f), Position=PointF(210.f,0.f), ForChoice = 4))
buttons.Add(new IUMButton(drawRect,Brush = Brushes.Black, Size = SizeF(50.f,50.f), Position=PointF(280.f,0.f), ForChoice = 5))


tb.KeyDown.Add(fun e ->
    
    match e.KeyCode with
    |Keys.Enter -> 
                   dim <- tb.Text
                   drawRect.Dimension <- int dim
                   drawRect.Invalidate()
                   lwc.Invalidate()
    |_ -> ()
    
)


for b in buttons do
    lwc.AddLWC b
f.Closing.Add(fun _ ->
    drawRect.StopTimer()
)
f.Show()
f.Focus()
