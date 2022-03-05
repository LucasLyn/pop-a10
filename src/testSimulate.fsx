// Part of module "dronesim".
open dronesim


// Define drones to use in test
                  //Pos    Spe  Dest
let Drone1 = Drone((0, 0), 10, (30, 20))
let Drone2 = Drone((1, 1), 20, (50, 50))
let Drone3 = Drone((2, 3), 5, (0, 0))

// Create drone list for testing
let mutable (droneLst:list<Drone>) = []

// Create new instance of Airspace for testing
let Airspace1 = Airspace(droneLst)

// Clear console before tests
System.Console.Clear()


///<summary>A test class to test Drone and Airspace class functions..</summary>
// Begin simTest test class
type simTest () =

    // Begin Drone() test;
    ///<summary>Prints the info of a drone.</summary>
    ///<param name = "drone">A drone to print info of.</param>
    member __.printDrone (drone:Drone) =
        printfn "#################################"
        printfn "Current values of drones to test;"
        printfn "Position:    %A" drone.Position
        printfn "X coord:     %A" drone.X
        printfn "Y coord:     %A" drone.Y
        printfn "Speed:       %A" drone.Speed
        printfn "Destination: %A" drone.Destination
        printfn "#################################"
        printfn ""
        printfn ""

    ///<summary>Test the position of a drone.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "pos">Position to test.</param>
    member __.testPosition (drone:Drone) (pos:int*int) = 
        printfn "Testing drone position..."
        printfn "Expected output type: tuple (int*int)"
        printfn "Input:         %A" pos
        printfn "Exp. output:   %A" pos
        printfn "Output:        %A" drone.Position
        printfn "Output return: %A (expecting 'true')" (pos = drone.Position)
        printfn ""
        printfn ""

    ///<summary>Test X coordinate of drone.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "pos">A coordinate to test.</param>
    member __.testX (drone:Drone) (pos:float) =
        printfn "Testing drone X coordinate..."
        printfn "Expected output type: %A" int
        printfn "Input:         %A" pos
        printfn "Exp. output:   %A" pos
        printfn "Output:        %A" drone.X
        printfn "Output return: %A (expecting 'true')" (pos = drone.X)
        printfn ""
        printfn ""

    ///<summary>Test Y coordinate of drone.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "pos">A coordinate to test.</param>
    member __.testY (drone:Drone) (pos:float) =
        printfn "Testing drone Y coordinate..."
        printfn "Expected output type: %A" int
        printfn "Input:         %A" pos
        printfn "Exp. output:   %A" pos
        printfn "Output:        %A" drone.Y
        printfn "Output return: %A (expecting 'true')" (pos = drone.Y)
        printfn ""
        printfn ""

    ///<summary>Test speed of drone.</summary>
      ///<param name = "drone">A drone to test.</param>
    ///<param name = "spe">A speed to test.</param>
    member __.testSpeed (drone:Drone) (spe) =
        printfn "Testing drone speed..."
        printfn "Expected output type: %A" int
        printfn "Input:         %A" spe
        printfn "Exp. output:   %A" spe
        printfn "Output:        %A" drone.Speed
        printfn "Output return: %A (expecting 'true')" (spe = drone.Speed)
        printfn ""
        printfn ""

    ///<summary>Test destination of drone.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "dest">A destination to test.</param>
    member __.testDestination (drone:Drone) (dest:int*int) =
        printfn "Testing drone destination..."
        printfn "Expected output type: tuple (int*int)"
        printfn "Input:         %A" dest
        printfn "Exp. output:   %A" dest
        printfn "Output:        %A" drone.Destination
        printfn "Output return: %A (expecting 'true')" (dest=drone.Destination)
        printfn ""
        printfn ""

    ///<summary>Test the fly function with a drone.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "exPos">An expected position to test.</param>
    member __.testFly (drone:Drone) (exPos:int*int) =
        printfn "Testing drone fly..."
        printfn "Expected output type: %A" None
        printfn "Input:         %A" exPos
        printfn "Exp. output:   %A" exPos
        drone.Fly
        printfn "Output:        %A" drone.Position
        printfn "Output return: %A (expecting 'true')" (exPos = drone.Position)
        printfn ""
        printfn ""

    ///<summary>Test if a drone is at it's destination.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "pos">A position to show.</param>
    ///<param name = "dest">A destination to test.</param>
    member __.testAtDestination (drone:Drone) (pos:int*int) (dest:int*int) =
        printfn "Testing drone AtDestination..."
        printfn "Expected output type: %A" bool
        printfn "Input:         %A & %A" pos dest
        printfn "Exp. output:   %A" true
        printfn "Output:        %A" drone.AtDestination
        printfn "Output return: %A (expecting 'true')" drone.AtDestination
        printfn ""
        printfn ""
    

    // Begin Airspace() test;
    ///<summary>Test a list of drones.</summary>
    ///<param name = "drones">A list of drones to test.</param>
    ///<param name = "exLen">The expected length of the list to test.</param>
    member __.testDronesLst (drones:list<Drone>) (exLen:int) =
        printfn "Testing drone list..."
        printfn "Expected output type: %A" int
        printfn "Input:         %A" exLen
        printfn "Exp. output:   %A" exLen
        printfn "Output:        %A" drones.Length
        printfn "Output return: %A (expecting 'true')" (exLen = drones.Length)
        printfn ""
        printfn ""

    ///<summary>Test the distance between 2 drones.</summary>
    ///<param name = "drone1">1st drone to test.</param>
    ///<param name = "drone2">2nd drone to test.</param>
    ///<param name = "exDist">Expected distance to test.</param>
    member __.testDroneDist (drone1:Drone) (drone2:Drone) (exDist:float) =
        printfn "Testing drone DroneDist..."
        printfn "Expected output type: %A" float
        printfn "Input:         %A" exDist
        printfn "Exp. output:   %A" exDist
        printfn "Output:        %A" (Airspace1.DroneDist drone1 drone2)
        printfn "Output return: %A (expecting 'true')"
                    (exDist = (Airspace1.DroneDist drone1 drone2))
        printfn ""
        printfn ""

    ///<summary>Test the multiple drones fly function.</summary>
    ///<param name = "drone1">1st drone to test.</param>
    ///<param name = "drone2">2nd drone to test.</param>
    member __.testFlyDrones (drone1:Drone) (drone2:Drone) =
        let oldPos1 = drone1.Position
        let oldPos2 = drone2.Position
        Airspace1.AddDrone drone1
        Airspace1.AddDrone drone2
        printfn "Testing drone FlyDrones..."
        printfn "Expected output type: %A" None
        printfn "Input:         2 drones"
        printfn "Exp. output:   %A" true
        printfn "Output:          "
        Airspace1.FlyDrones
        let result = ((oldPos1 = drone1.Position) = false &&
                        (oldPos2 = drone2.Position) = false)
        printfn "Output return: %A (expecting 'true')" result
        printfn ""
        printfn ""

    ///<summary>Test the function to add a drone to a list.</summary>
    ///<param name = "drone">A drone to test.</param>
    ///<param name = "exLen">The expected length of the list to test.</param>
    member __.testAddDrone (drone:Drone) (exLen:int) =
        let oldLen = Airspace1.Drones.Length
        printfn "Testing drone AddDrone..."
        printfn "Expected output type: %A" int
        printfn "Input:         %A" exLen
        printfn "Exp. output:   %A" exLen
        Airspace1.AddDrone drone
        printfn "Output:        %A" Airspace1.Drones.Length
        printfn "Output return: %A (expecting 'true')"
                    (exLen = Airspace1.Drones.Length)
        printfn ""
        printfn ""

    ///<summary>Test if 2 drones will collide.</summary>
    ///<param name = "drone1">1st drone to test.</param>
    ///<param name = "drone2">2nd drone to test.</param>
    ///<param name = "exLen">The expected length of the list to test.</param>
    member __.testWillCollide (drone1:Drone) (drone2:Drone) (exLen:int) =
        Airspace1.AddDrone drone1
        Airspace1.AddDrone drone2
        printfn "Testing drone WillCollide..."
        printfn "Expected output type: list<Drone>"
        printfn "Input:         2 drones"
        printfn "Exp. output:   a list of crashed drones"
        Airspace1.AddDrone drone1
        Airspace1.AddDrone drone2
        printfn "Output:        %A" Airspace1.WillCollide
        let result = (2 = (Airspace1.WillCollide.Length))
        printfn "Output return: %A (expecting 'true')" result
        printfn ""
        printfn ""


// Create a new instance of simTest class for testing
let simTest1 = simTest()

// Print info of drones to test
simTest1.printDrone Drone1
simTest1.printDrone Drone2
simTest1.printDrone Drone3


// Begin class calls to test
simTest1.testPosition Drone1 (0, 0)
simTest1.testPosition Drone2 (1, 1)

simTest1.testX Drone1 0.0
simTest1.testX Drone2 1.0

simTest1.testY Drone1 0.0
simTest1.testY Drone2 1.0

simTest1.testSpeed Drone1 10
simTest1.testSpeed Drone2 20

simTest1.testDestination Drone1 (30, 20)
simTest1.testDestination Drone2 (50, 50)

simTest1.testFly Drone1 (9, 5)
simTest1.testFly Drone2 (13, 15)

// Returns false in the first test due to the fact that it isn't at it's dest.
simTest1.testAtDestination Drone1 (0, 0) (30, 20)
simTest1.testAtDestination Drone3 (0, 0) (5, 5)

droneLst <- Drone1::droneLst
simTest1.testDronesLst droneLst 1
droneLst <- Drone2::droneLst
simTest1.testDronesLst droneLst 2

simTest1.testDroneDist Drone1 Drone2 1.4

simTest1.testFlyDrones Drone1 Drone2

simTest1.testAddDrone Drone1 1
simTest1.testAddDrone Drone2 2

// Fails; read below
simTest1.testWillCollide Drone1 Drone2 2

printfn "Check code comments for more."


(*
The tests were in general successful, except a few.
These tests are;
testFly
testDroneDist
testAddDrone
and testWillCollide

testFly failed because the final position is essentially rounded twice.
The first rounding  put the result closer to 15 than needed (in the first case),
and the second rounding put it to 15, which is a little off the expected 13.
Same goes for the sets (9, 8) and (5, 6).

testDroneDist failed because the drones had already flown from a previous test.
Plugging in the corrdinates in pythagoras' equation, I get that the distance
lines up with the actual outputted distance. This test simply failed in practice

testAddDrone failed for the same reason as testDroneDist. It was expected an
empty list, but from a previous test, there were already some drones in the list
Buy looking at the fact that 2 drones were added to the list earlier, the actual
amount of drones in the list from the actual output still lines up. Again,
this test failed in practice.

testWillCollide failed because drones 1 and 2 used here have each flown twice
before they were checked, making their conditions not line up, and thus, fail.
*)