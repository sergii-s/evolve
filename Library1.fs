module Model

open System
open System.Drawing
open System.Drawing.Imaging
open Rand

type Vertex = {
    x:int
    y:int
}
type Triangle = {
    a: Vertex
    b: Vertex
    c: Vertex
}
type ColoredPolygon = {
    Vertices: Triangle
    Color: Color
    ZIndex: int
}

type ZipImage = {
    Polygons: ColoredPolygon array
    SizeX: int
    SizeY: int
}

let bound min max value = 
    if value<min then 
        min
    else if value>max then 
        max
    else 
        value

let color a r g b = Color.FromArgb(a, r, g, b)
let randomColor () = color (rm 255) (rm 255) (rm 255) (rm 255)
let mutateByte (value:byte) = 
    let offset = ((rmd() - 0.5) * 255.) |> int
    printfn "v %i o %i" value offset
    ((value |> int) + offset) |> bound 0 255
let mutateFloat value = value * (rmd() - 0.5)
let moveVertex offsetX offsetY maxX maxY vertex = {x=(vertex.x+offsetX)|>bound 0 maxX;y=(vertex.y+offsetY)|>bound 0 maxY}
let moveTriangle offsetX offsetY maxX maxY triangle = 
    let mv = moveVertex offsetX offsetY maxX maxY
    {a = mv triangle.a;b= mv triangle.b;c=mv triangle.c }
 
let randomImage polygonCount sizeX sizeY : ZipImage =
    let randomVertex () = {x=rm sizeX; y=rm sizeY}   
    let polygons = Array.init polygonCount (fun i -> {Vertices = {a = randomVertex() ; b = randomVertex();c=randomVertex() }; Color = randomColor(); ZIndex=i})
    {
        Polygons = polygons
        SizeX = sizeX
        SizeY = sizeY
    }

let mutatePolygonRandom sizeX sizeY polygon = 
    //printfn "Mutate pol"
    let randomVertex () = {x=rm sizeX; y=rm sizeY}   
    let newColor =  Color.FromArgb(polygon.Color.A|>int,polygon.Color.R|> mutateByte, polygon.Color.G|> mutateByte,polygon.Color.B|> mutateByte)
    {Vertices = {a = randomVertex() ; b = randomVertex();c=randomVertex() }; Color = randomColor(); ZIndex=0}

let mutatePolygonColor polygon = 
    //printfn "Mutate ploygon color"
    let newColor =  Color.FromArgb(polygon.Color.A|>int,polygon.Color.R|> mutateByte, polygon.Color.G|> mutateByte,polygon.Color.B|> mutateByte)
    {Vertices=polygon.Vertices;Color=newColor;ZIndex=polygon.ZIndex}

let mutatePolygonAlpha polygon = 
    let newAlpha = polygon.Color.A |> mutateByte
    printfn "Mutate ploygon alpha to %i" newAlpha
    let newColor =  Color.FromArgb(newAlpha, polygon.Color.R|> int, polygon.Color.G|> int,polygon.Color.B|> int)
    {Vertices=polygon.Vertices;Color=newColor;ZIndex=polygon.ZIndex}

let mutatePolygonPositionGlobal (sizeX:int) (sizeY:int) polygon = 
    let offsetX = sizeX |> float |> mutateFloat |> int 
    let offsetY = sizeY |> float |> mutateFloat |> int 
    //printfn "Mutate ploygon position by x%i y%i" offsetX offsetY
    {Vertices=moveTriangle offsetX offsetY sizeX sizeY polygon.Vertices;Color=polygon.Color;ZIndex=polygon.ZIndex}

let mutatePolygonVertices sizeX sizeY polygon = 
    let mv vertex= 
        let offsetX = sizeX |> float |> mutateFloat |> int 
        let offsetY = sizeY |> float |> mutateFloat |> int 
        moveVertex offsetX offsetY sizeX sizeY vertex
    let tremble triangle = {a=mv triangle.a;b=mv triangle.b;c=mv triangle.c}
    {Vertices=tremble polygon.Vertices;Color=polygon.Color;ZIndex=polygon.ZIndex}

let mutate sizeX sizeY = 
    let mutateMethods = [|mutatePolygonColor;mutatePolygonAlpha;mutatePolygonRandom sizeX sizeY; mutatePolygonPositionGlobal sizeX sizeY;mutatePolygonVertices sizeX sizeY|]
    fun polygon -> 
        let choosedMutation = mutateMethods |> (mutateMethods.Length |> rm |> Array.item) 
        choosedMutation polygon

let mutateImage mutationChance (zipImage:ZipImage) = 
    let mutate = mutate (zipImage.SizeX) (zipImage.SizeY)
    let polygons = zipImage.Polygons
    for i in 0 .. polygons.Length-1 do
        if rmd() < mutationChance then 
            polygons.[i] <- (polygons.[i] |> mutate)

let crossImages male female =
    let crossPolygons p1 p2 =
        Array.map2 (fun x y -> rmof x y) p1 p2
    {Polygons = crossPolygons male.Polygons female.Polygons; SizeX=male.SizeX; SizeY = male.SizeY}

let toPoints vertices =
    let toPoint v = Point(v.x, v.y)
    [|vertices.a|>toPoint;vertices.b|>toPoint;vertices.c|>toPoint|]

let toBitmap zipImg =
    let generatedImg = new Bitmap(zipImg.SizeX, zipImg.SizeY)
    use gr = Graphics.FromImage(generatedImg)
    for polygon in zipImg.Polygons |> Array.sortBy(fun p -> p.ZIndex) do
        use brush = new SolidBrush(polygon.Color)
        gr.FillPolygon(brush, polygon.Vertices |> toPoints)
    generatedImg

let toByteArray (bitmap:Bitmap) =
    let bData = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, bitmap.PixelFormat)
    let size = bData.Stride * bData.Height
    let data = Array.zeroCreate<byte> size
    System.Runtime.InteropServices.Marshal.Copy(bData.Scan0, data, 0, size)
    bitmap.UnlockBits(bData)
    data

let distance (data1:byte[]) (bm:Bitmap) =
    let data2 = bm |> toByteArray
    let getIDist i = 
        let getSqr j = 
            let idx = i*4+j
            let diff = ((data1.[idx]-data2.[idx]) |> double) / 255.
            diff*diff
        (getSqr 0) + (getSqr 1) + (getSqr 2) + (getSqr 3)
    [|0..data1.Length/4-1|] |> Array.Parallel.map(fun i -> getIDist i) |> Array.sum
            
let fitness (refImg:byte[]) zipImg =
    let generatedImg = zipImg |> toBitmap
    distance refImg generatedImg

    
let saveLeader iterNumber img = 
    let bitmap = toBitmap img
    bitmap.Save(@"C:\Temp\gen\0.png", ImageFormat.Png)
    




let refFilePath = @"C:\Temp\test.png"
let generationSize = 40
let survivalCount = 2
let mutationChance = 0.25
let polygonCount = 20
let img = new Bitmap(refFilePath)
let refImgBytes = img |> toByteArray
printfn "w:%i h:%i size:%i" img.Width img.Height refImgBytes.Length


let firstGeneration = Array.init generationSize (fun x->randomImage polygonCount img.Size.Width img.Size.Height)
    
let enstinctGeneration generation =
    generation 
        |> Array.Parallel.map(fun zipImg -> fitness refImgBytes zipImg, zipImg)
        |> Array.sortBy(fun (fitnessRes, zipImg) -> fitnessRes)
        |> Array.take(survivalCount)
        |> Array.map(fun (fitnessRes, zipImg) -> zipImg)
        

let populateGeneration generation =
    let newMemebersCount = generationSize - Array.length generation
    let newMembers = Array.init generationSize (fun x -> crossImages generation.[0] generation.[1]) 
    newMembers |> Array.iter(fun zipImage -> mutateImage mutationChance zipImage)
    Array.concat [generation; newMembers]

let rec oneGen generation iter = seq {
    let survived = enstinctGeneration generation
    survived |> Array.head |> saveLeader iter
    let nextGeneration = populateGeneration survived
    yield nextGeneration
    yield! oneGen nextGeneration (iter+1)
}

oneGen firstGeneration 0 |> Seq.item Int32.MaxValue |> ignore
    