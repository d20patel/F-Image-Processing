//
// F# image processing functions.
//
// Name: Deep Patel
// School: University of Illinois at Chicago
// Semester: Fall 2022
// 

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //

  // helper for creating tuple for various functions
  let _TupleRGB a b c =
    let theTuple = (a, b, c)
    theTuple

  // helper for calculating the gray pixels and then making tuple
  let rec _TheGray theList =
    match theList with
    |[] -> []
    |hd::tl -> 
        match hd with
        | (a, b, c) ->
            let a1 = (0.299*float(a))
            let b1 = (0.587*float(b))
            let c1 = (0.114*float(c))
            let avg = int(a1 + b1 + c1)
            let grayPixel = _TupleRGB avg avg avg
            grayPixel :: _TheGray tl

  // iterates list of lists containing tuples as pixels to implement Grayscale utilizing _TheGray
  let rec _GrayLists theList =
    match theList with
      |[] -> []
      |hd::tl -> _TheGray hd :: _GrayLists tl

  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) =

    let finalGray = _GrayLists image  // returns resulting gray image as list of list
    finalGray


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //

  // helper for checking if the value for a,b,c is more or less than Threshold
  // also returns an appropriate tuple according to the condition that is matched for a,b,c
  let _ThresholdCheck a b c theThreshold theDepth = 
    if (a <= theThreshold && b <= theThreshold && c <= theThreshold) then
      let theRGB = _TupleRGB 0 0 0
      theRGB
    else if (a > theThreshold && b <= theThreshold && c <= theThreshold) then
      let theRGB = _TupleRGB theDepth 0 0
      theRGB
    else if (a > theThreshold && b > theThreshold && c <= theThreshold) then
      let theRGB = _TupleRGB theDepth theDepth 0
      theRGB
    else if (a > theThreshold && b <= theThreshold && c > theThreshold) then
      let theRGB = _TupleRGB theDepth 0 theDepth
      theRGB
    else if (a <= theThreshold && b > theThreshold && c <= theThreshold) then
      let theRGB = _TupleRGB 0 theDepth 0
      theRGB
    else if (a <= theThreshold && b > theThreshold && c > theThreshold) then
      let theRGB = _TupleRGB 0 theDepth theDepth
      theRGB
    else if (a <= theThreshold && b <= theThreshold && c > theThreshold) then
      let theRGB = _TupleRGB 0 0 theDepth
      theRGB
    else
      let theRGB = _TupleRGB theDepth theDepth theDepth
      theRGB

  // iterates through the tuples in the list to implement _ThresholdCheck
  let rec _IterateTuple theList theThreshold theDepth =
    match theList with
    |[] -> []
    |hd::tl -> 
        match hd with
        | (a, b, c) -> _ThresholdCheck a b c theThreshold theDepth :: (_IterateTuple tl theThreshold theDepth)

  // iterates all the lists to implement _IterateTuple
  let rec _IterateList theList theThreshold theDepth =
    match theList with
    |[] -> []
    |hd::tl -> (_IterateTuple hd theThreshold theDepth ) :: (_IterateList tl theThreshold theDepth)

  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    
    let finalThreshold = _IterateList image threshold depth  // returns the an updated threshold pixels
    finalThreshold


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //

  // for flipping the values inside tuple that is the pixel
  let rec _IterateFlip theList =
    match theList with
    |[] -> []
    |hd::tl ->
        match hd with
        | (a, b, c) -> _TupleRGB a b c :: _IterateFlip tl

  // iterates the list to implement the _IterateFlips for tuples containing pixel values
  let rec _IterateFlips theList=
    match theList with
      |[] -> []
      |hd::tl -> (_IterateFlip hd) :: (_IterateFlips tl)

  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 

    let finalList = _IterateFlips image
    let horizontalFlipped = finalList |> List.map List.rev  // reverses the list
    horizontalFlipped


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // scanEdge to calculate the distance of a particular pixel with its right pixel and below pixel
    let scanEdge thePixel theRight theBelow =
        let (a,b,c) = thePixel
        let (a1,b1,c1) = theRight
        let (a2,b2,c2) = theBelow
        let rightDistance = (sqrt (float((float a - float a1)**2.0 + (float b - float b1)**2.0 + (float c - float c1)**2.0)))
        let belowDistance = (sqrt (float((float a - float a2)**2.0 + (float b - float b2)**2.0 + (float c - float c2)**2.0)))
        if (rightDistance > float threshold || belowDistance > float threshold) then
            (0, 0, 0)  // black pixel since there's an edge
        else
            (255, 255, 255)  // white pixel for no edge

    // iterates and assigns its right and below pixel to implement the scanEdge for each pixel on the image
    let rec scanEdgeDetect thePixels nxtRow = 
        match thePixels with
        | [] -> []
        | hd::tl -> if tl.IsEmpty then
                        []
                    else
                        let rightPixel = List.head tl
                        let belowPixel = List.head nxtRow
                        (scanEdge hd rightPixel belowPixel) :: scanEdgeDetect tl (List.tail nxtRow)

    match image with
    | [] -> []
    | hd::tl -> if List.length image = 1 then
                    []
                else
                    (scanEdgeDetect hd (List.head tl)) :: EdgeDetect width height depth tl threshold


 
  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    match image with
    | [] -> []
    | hd::tl -> if hd.IsEmpty then
                    []
    // since not empty, rotates the image 90 degree utilizing recursion for replacing pixels in the list to return 90 degree rotated image 
                else
                    (List.rev (List.map (fun E -> List.head E) image)) :: RotateRight90 width height depth (List.map (fun E -> List.tail E) image)