# erl-cqrs

With my growing passion for CQRS, Event Sourcing, Domain Driven Design and Erlang in the past few months, I have set myself a challenge of implementing a simple, yet broad enough system of CQRS+ES using Erlang. This repository is the intention of materializing that challenge.

The goal is to create a simple system that receives strings as input and allows you to know how many strings have been inputed, how many of them are distinct, and maybe some other information.

The goal is to have a command **EmitString(*Str*)** that gets handled by a command handler.
This command handler is responsible for the integrity of the data in the system regarding these strings. For now we will have two fictitious "business requirements":

* Each word on the input string can have at most one uppercase letter;
* Once 100 characters have been inputed into the system, the only valid string after that is "carriageReturn". Inputting "carriageReturn" will allow the introduction of 100 more characters.

## Example of usage (language and syntax agnostic):
```
>A string that has nothing wrong with it
ok.

>A string that contains ONE word that violates a business constraint;
error: Each word can have at most one uppercase letter.

>The first string was 39 characters long. This one is 305. This is quite long, so the system should go into expecting a carriage return. Keep in mind that this string can exceed the 100 characters, that is not an issue. Only once we have exceeded 100 characters does the system go into carriage return mode
ok.

>Anything other than just "carriageReturn" should fail now
error: expecting carriage return.

>carriageReturn
ok.

>funny enough, but the 14 characters of "carriageReturn" should not be considered as having been inputed into the system
ok.
```

The example above tries to be as minimal as possible. It does not use HTTP, it does not use JSON, it does not use telnet, etc. Those are implementation details, which should be abstracted away from the requirements of the system. They will be contemplated when the time is right.

## Obtaining information from the system

One of the great benefits from CQRS is that you get to separate the strategies you used to guarantee the consistency of your system, from the techniques of obtaining information from your system.

For this repository, we have different needs for data. They are obviously fictitious, but they should be complex enough to allow us to dive into the concept of projections/denormalizers/read models (call them what you will).

* Knowing how many **characters** have been inputted into the system:
  * The input `hello world` has 11 **characters**;
  * The special string `carriageReturn`, **when inputted to start a new input session** does not count as characters inputted into the system. Otherwise it counts as its 14 characters;
* Knowing how many **words** have been inputted into the system:
  * The input `hello world` has 2 **words**;
  * The input `hello-cruel+world` has 1 **word**;
  * The input `The number 5 is awesome` has 5 **words**;
  * Basically only a space ([ascii](http://ascii.cl/) 32) splits words appart;
* Knowing how many input sessions have occurred so far:
  * An input session ends once 100 or more characters have been inputed into the system;
  * An input session starts with the `carriageReturn` string;
  * When `carriageReturn` is inputted to start a new session, its characters are not counted for the total characters inputted into the system, nor does it count as a word;

## Technical challenges

* Persistence of the events: the server/erlang VM is allowed to be shutdown. When started, given the physical media still exists, the system should get itself into the correct state. This includes both the read side and the command side;