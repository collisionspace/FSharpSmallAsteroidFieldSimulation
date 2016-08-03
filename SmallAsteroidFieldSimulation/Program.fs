(*
In this chapter we will see how we can manipulate long sequences of data.
We will show how we can fill, modify, iterate and fold a list.
*)

(*
We will build a simple asteroid field simulation, which computes the various forces a series of asteroids apply to each other.
We will reuse the Mmath library defined in chapter 2, in particular we will use the units of measure defined there and the Vector2 record.

Before we dive right in the code, we will (as we did in the previous chapters) gather what we know about the problem:
- the system we simulate is comprised of a series (a list) of asteroids
- each asteroid has a position, a velocity and a mass
- two asteroids attract each other with a gravitational force of G * m1 * m2 * dir / dist^3, where dir = p1 - p2 for the second asteroid and dir = p2 - p1 for the first asteroid; dist is the length of dir
- when an asteroid touches the border of the field we will make it bounce it: we are simulating an "asteroid box", rather than an "asteroid field"
*)
module Sim = 
  (*
  Our simple math module contains basic units of measure and a definition for bidimensional vectors (we will simulate the motion in 2 dimension).
  *)
  module Math =
    (*
    We define a new type with the "type" keyword. The simplest types we can define are types with no definition, which are just placeholders.
    Such types are defined as units of measure, and are preceded by the [<Measure>] attribute.

    We define m (meters), kilograms (kg) and seconds (s).
    *)
    [<Measure>]
    type m

    [<Measure>]
    type kg

    [<Measure>]
    type s

    (*
    Units of measure may be composite. For example, we could define newtons (N) as the unit of measure for force, that is mass times acceleration or kg * m / s^2
    *)
    [<Measure>]
    type N  = kg * m / s^2

    (*
    Units of measure are very simple types, but they do not serve much porpose apart from classifying values of the same type into different groups; for example, we could
    try and sum 2.0<m> + 1.0<s> and the F# compiler would give us an error.

    A more interesting type definition is that of records. Records are very similar to tuples, but with a main difference: the elements (fields) of a record have a name.
    We could, of course, define a vector as float * float. Unfortunately, it would be very easy to make the mistake of losing track of which element is the x and which is the y.
    A record does exactly this: it keeps track of "which element of the tuple is which".

    Any definition of a type can contain a unit of measure, right after its name, between angle brackets. This means that such a type will need to be handled by also giving its values a unit of measure.
    We could avoid doing so, but for a complex physics simulation the advantage of having the compiler track what is in meters and what is in meters per second is quite important.
    
    A simple record definition would be:

    type Vector2 = { X : float; Y : float }

    where a record with units of measure would be defined as follows. Notice that the fields of the record are not simple floating point numbers, but rather they are floating point numbers with the same unit of measure of the record itself:
    *)
    type Vector2<[<Measure>] 'a> =
      {
        X : float<'a>
        Y : float<'a>
      }
      (* 
      A record may have members, which are functions that perform computations on values of type record.
      The simplest members of a record are static.
      Static members are defined with the syntax:
      static member Name : TYPE = BODY
      
      A very useful static member is the zero vector, which returns us a vector with all components initialized to zero.
      Notice that this static member must return a vector with a generic unit of measure 'a ('a may be any valid unit of measure: m, kg, s, m / s, etc.).
      For this reason the fields of the returned record must have the same unit of measure of the vector, as per the above definition.
     
      We invoke a static member by writing, for example:
      let v = Vector2<m/s>.Zero

      where v : Vector2<m/s>      
      *)     
      static member Zero : Vector2<'a> = { X = 0.0<_>; Y = 0.0<_> }

      (*
      We may sum two vectors, as long as they have the same units of measure. To sum them, we build a new record where each field is the sum of the two records.
      We may also sum a vector and a scalar.

      Given two vectors v1 : Vector2<m> and v2 : Vector2<m> we sum them by writing v1 + v2. The result will have type Vector2<m>.
      *)
      static member ( + ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = { X = v1.X + v2.X; Y = v1.Y + v2.Y }
      static member ( + ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = { X = v.X + k; Y = v.Y + k }
      static member ( + ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = v + k
      
      (*
      We may negate a vector v by writing -v; this operation is called ~-; in general, an operator prefixed with ~ is a unary operator, that is it takes just one parameter.
      Negating a vector with unit of measure 'a returns another vector with the same unit of measure:
      *)
      static member ( ~- ) (v:Vector2<'a>):Vector2<'a> = { X = -v.X; Y = -v.Y }
      
      (*
      We may subtract vectors and scalars:
      *)
      static member ( - ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = v1 + (-v2)
      static member ( - ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = v + (-k)
      static member ( - ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = k + (-v)
 
      (*
      We may multiply vectors and scalars. Multiplying a vector with unit of measure 'a with another vector (or scalar) with another unit of measure 'b produces a vector with unit of measure <'a * 'b>.
      For example, if v1 : Vector2<m> and v2 : Vector2<kg> then v1 * v2 : Vector2<m * kg>:
      *)
      static member ( * ) (v1:Vector2<'a>,v2:Vector2<'b>):Vector2<'a * 'b> = { X = v1.X * v2.X; Y = v1.Y * v2.Y }
      static member ( * ) (v:Vector2<'a>,f:float<'b>):Vector2<'a * 'b> = { X = v.X * f; Y = v.Y * f }
      static member ( * ) (f:float<'b>,v:Vector2<'a>):Vector2<'b * 'a> = { X = f * v.X; Y = f * v.Y }
      (*
      We may divide a vector by a scalar. The resulting vector will have a unit of measure that is the ratio of the units of measure of the vector and the scalar.
      For example, if v : Vector2<m> and t : float<s> then v / s : Vector2<m/s>:
      *)
      static member ( / ) (v:Vector2<'a>,f:float<'b>):Vector2<'a / 'b> = v * (1.0 / f)

      (*
      We can define members that can be applied on a particular vector. These members are called instance members, because they require a valid instance of the type to be invoked.
      Instance members are defined with the syntax:
      member SELF.NAME : TYPE = BODY

      Programmers with an object-oriented background may be used to using the name "this" for SELF.
      A very useful member for our vector computes the length of a vector. Notice that the result is a floating point number with the same unit of measure of the original vector.
      To compute the length of a vector v, we simply write v.Length:
      *)
      member this.Length : float<'a> = sqrt((this.X * this.X + this.Y * this.Y))

      (*
      We use vector subtraction and the length member to define the static member that computes the distance between two vectors and the normalization of a vector:
      *)
      static member Distance(v1:Vector2<'a>,v2:Vector2<'a>) = (v1-v2).Length
      static member Normalize(v:Vector2<'a>):Vector2<1> = v / v.Length

      member this.Normalized = this / this.Length

      static member Dot(v1:Vector2<'a>,v2:Vector2<'a>) = v1.X * v2.X + v1.Y * v2.Y


  module SmallAsteroidFieldSimulation =

    open System
    open System.Threading

    open Math

    (*
    An asteroid is a record, and it is comprised of a position (in meters), a velocity (in meters per second), a mass (in kilograms) and a name for printing.
    *)
    type Asteroid =
      {
        Position : Vector2<m>
        Velocity : Vector2<m/s>
        Mass     : float<kg>
        Name     : string
      }

    (*
    We declare a set of constants that characterize our simulation.
    dt is the amount of time we simulate at each step, G is the gravitational constant and the various other constants are used to define (large) asteroids with mass between the Earth and Moon masses.

    The size of the asteroid field is quite small: it fits entirely within the distance between the Earth and the Moon.
    This way the gravitational forces should be unrealistically intense but quite spectacular to look at!
    *)
    let dt = 60.0<s>
    let G = 6.67e-11<m^3 * kg^-1 * s^-2>

    let earth_radius = 6.37e6<m>
    let field_size = earth_radius * 60.0
    let max_velocity = 5.3e4<m/s>
    let earth_mass  = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>

    (*
    We define a function that initializes an asteroid field of an arbitrary number of asteroids:
    *)
    let create_field num_asteroids =
      (*
      We define a useful function, called a linear interpolation ("LERP", or "Linear intERPolation", who knows who picks these names!) that mixes together two values x and y according to
      a coefficient alpha. If alpha is 0 then we get one of the values; if alpha is 1 then we get the other value; if alpha is in between then we get a mix of the two proportional to the value of alpha.
      Notice that to be able to use x and y with units of measure inside the lerp function then we have to explicitly generic) type annotations that say that both x and y have the same unit of measure:
      *)
      let lerp (x:float<'u>) (y:float<'u>) (a:float) = x * a + y * (1.0 - a)
      (*
      We instance a value of type System.Random. F# may access any .Net datatype, even if written in another language such as C# or VB.Net.
      We invoke the constructor of a datatype without the usual keyword "new", which we may optionally provide if so we wished.
      In particular we invoke the constructor of the System.Random datatype to obtain a random number generator:
      *)
      let rand = Random()
      (*
      We can now instance 20 random asteroids.
      We define a list between square brackets.
      The shortest possible list is the empty list: []
      A list of integers with just one element would be [1]
      A list of strings with various elements is written ["hello"; "world"]

      A far more powerful way to manipulate lists would be to use list-comprehension syntax.
      We can define a list with a mixture of loops and the yield keyword.
      The idea is that we write a piece odf code, within brackets. This piece of code is executed, and every time the expression "yield x" is encountered then the value of x is added to the list.
      *)
      [
        for i = 1 to num_asteroids do
          (*
          To create an asteroid, we generate a random mass, a random position and a random velocity.
          The mass is between the Earth and Moon masses.
          The position is somewhere within the field.
          The velocity is between -max_velocity and +max_velocity, where max_velocity is what it would take to go from one side to the other of the entire field in a reasonable amount of time (a few minutes, that is a few ticks of the simulation).
          *)
          let m = (lerp earth_mass moon_mass (rand.NextDouble())) * 1.0e-4
          let x = lerp 0.0<m> field_size (rand.NextDouble())
          let y = lerp 0.0<m> field_size (rand.NextDouble())
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          (*
          When we are done initializing the values that characterize the asteroid we create it by assigning its fields and then we yield it, adding it to the list.
          We can either use "a" as a name for the asteroid, or a random character to make visually tracking the asteorids much simpler.
          *)
          yield
            {
              Position = { X = x; Y = y }
              Velocity = { X = vx; Y = vy }
              Mass     = m
              Name     = string(char((int 'a') + rand.Next(27)))
            }

      ]

    (*
    The initial asteroid field contains 20 randomized asteroids.
    *)
    let f0 = create_field 28

    (*
    We create two auxiliary functions that we will use in a similar simulation in the next chapter.

    We start with an auxiliary function that forces asteroids to bounce on the field borders.
    If the position exits from the field borders, then we force the asteroid back inside and modify its velocity towards the inside of the field:
    *)
    let clamp (p:Vector2<_>,v:Vector2<_>) =
      let p,v =
        if p.X < 0.0<_> then
          { p with X = 0.0<_> }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.X > field_size then
          { p with X = field_size }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.Y < 0.0<_> then
          { p with Y = 0.0<_> }, { v with Y = -v.Y }
        else p,v
      let p,v =
        if p.Y > field_size then
          { p with Y = field_size }, { v with Y = -v.Y }
        else p,v
      p,v

    (*
    We define another auxiliary function that computes the force between two asteroids a and a' with the known equation:
    *)
    let force (a:Asteroid,a':Asteroid) =
      let dir = a'.Position - a.Position
      let dist = dir.Length + 1.0<m>
      G * a.Mass * a'.Mass * dir / (dist * dist * dist)

    (*
    A step of the simulation updates each asteroid, according to the following rules:
    - an asteroids keeps moving along its velocity
    - an asteroid bounces on the borders of the field
    - an asteroid is subject to gravitational attraction from all the other asteroids
    *)
    let simulation_step (asteroids:Asteroid list) =
      (*
      We iterate all asteroids, and apply their velocity, bouncing and the various gravitational forces to each one.
      *)
      [
        for a in asteroids do
          (*
          We find the list of all the forces that the other asteroids apply on a. We check if two asteroids are the same with the <> operator:
          *)
          let forces =
               [
                 for a' in asteroids do
                   if a' <> a then
                     yield force(a,a')
               ]
          (*
          The final force that is applied on the current asteroid a is the sum of all the forces from the various asteroids:
          *)
          let F = List.sum forces
          (*
          We compute the effects of bouncing with the clamp function, and then we yield the updated asteroid by increasing its position by its velocity and its velocity by its acceleration.
          *)
          let p',v' = clamp(a.Position,a.Velocity)
          yield
            {
              a with
                  Position = p' + dt * v'
                  Velocity = v' + dt * F / a.Mass
            }
      ]

    (*
    The printing and simulation functions are almost the same we have seen in Chapter 2.
    *)
    let print_scene (asteroids:Asteroid list) =
      do Console.Clear()
      for i = 0 to 79 do
        Console.SetCursorPosition(i, 0)
        Console.Write("*")
        Console.SetCursorPosition(i, 23)
        Console.Write("*")
      for j = 0 to 23 do
        Console.SetCursorPosition(0,j)
        Console.Write("*")
        Console.SetCursorPosition(79,j)
        Console.Write("*")
      let set_cursor_on_body b =
        Console.SetCursorPosition(((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int, ((b.Position.Y / 4.0e8<m>) * 23.0 + 1.0) |> int)
      for a in asteroids do
        do set_cursor_on_body a
        do Console.Write(a.Name)
      do Thread.Sleep(100)

    let simulation() =
      let rec simulation m =
        do print_scene m
        let m' = simulation_step m
        do simulation m'
      do simulation f0

    [<EntryPoint>]
    let main argv = 
       simulation()
       printfn "%A" argv
       0 // return an integer exit code

