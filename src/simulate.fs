// Part of the module "dronesim".
module dronesim


// Drone class with different properties that define some standard stuff.
///<summary>Drone class with some Drone related properties like speed</summary>
///<param name = "Position">A tuple of a drone's current position.</param>
///<param name = "Speed">A drone's speed in cm per second.</param>
///<param name = "Destination">The destination a drone is traveling to.</param>
type Drone (Position:int*int, Speed:int, Destination:int*int) =
    let mutable (posX, posY) = Position
    let mutable spe = Speed
    let mutable (destX, destY) = Destination

    ///<summary>Get a drone's psoition.</summary>
    ///<returns>A drone's current position in a tuple.</returns>
    member __.Position = (posX, posY)

    ///<summary>Get a drone's current X coordinate.</summary>
    ///<returns>A drone's current X coordinate.</returns>
    member __.X = float posX

    ///<summary>Get a drone's current Y coordinate.</summary>
    ///<returns>A drone's current Y coordinate.</returns>
    member __.Y = float posY

    ///<summary>Get a drone's speed.</summary>
    ///<returns>A drone's speed.</returns>
    member __.Speed = spe

    ///<summary>Get a drone's current destination.</summary>
    ///<returns>A drone's current destination.</returns>
    member __.Destination = (destX, destY)

    ///<summary>Makes a drone fly towards it's destination.</summary>
    member __.Fly =
        let v = atan2 (float (destY - posY)) (float (destX - posX))
        if (destX - posX) <= spe && (destY - posY) <= spe then
            posX <- destX
            posY <- destY
        else
            posX <- posX + int (round (cos (v) * float (spe)))
            posY <- posY + int (round (sin (v) * float (spe)))
    
    ///<summary>Checks if a drone is at it's target destination.</summary>
    ///<returns>A boolean stating if the drone is at it's given dest.</returns>
    member __.AtDestination = ((posX, posY) = (destX, destY))




// Airspace class with different drone functions.
///<summary>An Airspce class with drone functions.</summary>
///<param name = "Drones">A list of Drones.</param>
type Airspace (Drones:list<Drone>) =
    let mutable drones = Drones

    ///<summary>A list of Drones.</summary>
    ///<returns>A list of Drones.</returns>
    member __.Drones = drones

    ///<summary>Calculates the distance between 2 drones.</summary>
    ///<param name = "drone1">First drone to calculate distance of.</param>
    ///<param name = "drone2">Second drone to calculate distance of.</param>
    ///<returns>A float tuple of the distance between 2 given drones.</returns>
    member __.DroneDist (drone1:Drone) (drone2:Drone) : float =
        let a = drone2.X - drone1.X
        let b = drone2.Y - drone1.Y
        round (sqrt(float (a ** 2.0) + float (b ** 2.0)))
    
    ///<summary>Makes all drones in the collection of drones fly.</summary>
    member __.FlyDrones =
        for i in 0 .. drones.Length - 1 do
            drones.[i].Fly
    
    ///<summary>Adds a given drone to the list of drones.</summary>
    ///<param name = "drone">A drone to add to the list of drones.</param>
    member __.AddDrone drone =
        drones <- drone::drones

    // If 3 drones are to collide, the 3rd drone will be lucky and miss the
    // other drones barely.
    ///<summary>Checks if 2 drones will collide.</summary>
    ///<returns>A list of crashed drones.</returns>
    member __.WillCollide =
        let mutable crashedDrones = []
        let mutable drone1 = __.Drones.[0]
        let mutable drone2 = __.Drones.[0]
        let mutable dist = float (__.DroneDist drone1 drone2)
        for i in 0 .. __.Drones.Length - 2 do
            drone1 <- __.Drones.[i]
            for j in i + 1 .. __.Drones.Length - 1 do
                drone2 <- __.Drones.[j]
                dist <- float (__.DroneDist drone1 drone2)
                if dist < 5.0 && dist<>0.0 && __.Drones.[i].AtDestination<>true
                            && __.Drones.[j].AtDestination <> true then // ln 95
                    crashedDrones <- (__.Drones.[i],
                                __.Drones.[j])::crashedDrones // Ext. of ln 97
        crashedDrones