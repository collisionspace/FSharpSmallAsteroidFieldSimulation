(*
In this chapter we will see how we can organize our data in F#.
We will show how we can organize information in records, define functions associated with a datatype and even mark values with units of measure.
*)

(*
We will build a simple rocket simulation, which computes the various forces a Saturn V rocket (those used in the Apollo missions) is subject to.
We will start with the definition of a set of units of measure such as kilograms, meters, seconds, etc. to make sure we do not mix values in a wrong way (for example adding seconds to kilograms).
We will create a small library for manipulating vectors, and then we will define our simulation.

Before we dive right in the code, we will (as we did in the first chapter) gather what we know about the problem:
- the system we simulate is comprised of three bodies: the Earth, the Moon and the rocket
- the rocket is subject to the gravitational attraction of both the Moon and the Earth
- a body has (at least) a mass m and a position p
- two bodies attract each other with a gravitational force of G * m1 * m2 * dir / dist^3, where dir = p1 - p2 for the second body and dir = p2 - p1 for the first body; dist is the length of dir
- the rocket has three stages, each filled with fuel and capable of pushing the rocket with a certain force; when a stage has no more fuel, it is detached and the next stage is turned on
*)
namespace Chapter2
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
    type N = kg * m / s^2

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

