---
draft: true
title: Functors - Redux
description: >
  Last year I wrote an article
  explaining functors in simple terms and show why they are useful. It has well
  received, but some felt it was lacking in details which is true and by design.
  There is more to be said and some crucial details that I glossed over so I
  will try to address some of them now. Let’s get nerdy!

---

*This article has first published as a part of [Bekks 2020 christmas calendar](https://www.bekk.christmas/post/2019/20/functors-what-are-they)*

Last year I wrote [an article](https://functional.christmas/2019/20)
explaining functors in simple terms and show why they are useful. It has well
received, but some felt it was lacking in details which is true and by design.
There is more to be said and some crucial details that I glossed over so I
will try to address some of them now. Let’s get nerdy!

# Functor as a context

In the previous article I said you could think of a functor as a structure or container that has a mapping function. This is not strictly wrong as it is one way of thinking about the concept, but can be a bit misleading and confusing as it does not fit all situations. For example a functions can in some cases be a functor. A structure might give us thoughts about data structures or types. Container is fitting when talking about lists or other types that can contain data. Is a function a structure or container? For this reason it is common to refer to some of the general concepts and abstractions we use in FP (functor, monad etc.) as contexts which has some properties.

# The functor laws

Unlike my vague explanation in the previous article a functor is actually a very specific thing, and is defined by rules or properties the context has to adhere to. These laws come from mathematics, specifically a branch called Category Theory. This is an area that I will not claim much knowledge of but great minds have found similarities between this particular part of mathematics and what we do in programming. 

In addition to having the `map` function, functors needs to follow some rules.
The rules for functors are often called the functor laws. I'm not completely sure why. It might have something to do with math and also its sounds very sophisticated. :P Let’s get in on the sophistication!

## 1. Law of preservation of identity

The first law or rule of functors is that if the `map` function is given the identity function as its mapping function it will return the same functor. 
The identity function is just a function that has one argument which it returns right back (`\x -> x` in Elm or `function(x){return x;}` in JS).

Let’s use lists to visualize this a bit more. Elm actually has the identity function in its standard library so we will just use that one:

```elm
List.map identity [1,2,3,4] == [1,2,3,4]
```

For the javascript example we will add the identify function inline:

```javascript
[1,2,3,4].map((x) => x) == [1,2,3,4]
```

When following this law the functor can not make any weird changes to the result. `List.map someFunction [1,2,3,4]` can never return an empty list. It can only map the function over the values. When mapping over a 4 item list, the returning list will always be a 4 item list. 

## 2. Law of composition

The second law is not very complicated either but assumes that you know about composition. Lets quickly go through composition before looking at the next law.

### Composition

Composition in functional programming (FP) is the ability to compose multiple functions into one single function. This is not often used in object oriented programming (OOP) but quite common in FP. 

Composition is "gluing" functions together into one function. This is such a common thing to do that many languages have dedicated operators for this. Lets look at an example to see it in action to see why this might be useful. I'll be doing the examples in Elm as composition is a bit awkward in javascript and not that common.

Lets say you have a user record that has the fields name and age:

```elm
type alias User = 
    { name : String
    , age: Int
    }
```

To get the age of a User record we can in Elm use an accessor function which is available for every field defined in a record. <fn-ref id="accessor"/> For the `age` field this function is called `.age` and has the type signature `User -> Int`. It takes a `User` type as an argument and returns a `Int`. In this case it is the content of the age field.

Go get the age from a user you would to: 

```elm
-- Lets first define a User record to work with
user = { name = "John", age = 20 }

-- Get age
.age user
-- output: 20
```

At some point in your program you might have a `User` and need to display the age on a webpage, print it in a log or anything else that needs the age to be a string. To convert a `Int` into a `String` we can use the built-in function `fromInt` in the String module. 

So where you have a user and need the age as a string you can use the compose operator in Elm to create a new function that gives you the user age as a string. I will write the type signatures for the functions as well so we can see how they fit together: 

```elm
.age : User -> Int

fromInt: Int -> String

ageAsString : User -> String
ageAsString = .age >> fromInt
```

When the output of a function matches the input of another we can compose them together and make a new function that has the input of the first and output of the second. Note that `>>` is the rightwards compose operator. <fn-ref id="composeop" />

### The second law

Now that you hopefully know a bit about function composition lets look at the second law of functors. The rule says that composing functions into a single function and then mapping that function over the functor should produce the same result as mapping the individual functions over the functor in sequence. Lets look as some code to make sense of it:

```elm
users : List User
users = 
    [ { name = "John", age = 20 }
    , { name = "Mary", age = 22 }
    , { name = "Kevin", age = 30 }
    ]

agesThroughMaps : List String
agesThroughMaps = 
    users
        |> List.map .age
        |> List.map String.fromInt

agesThroughComposeAndMap : List String
agesThroughComposeAndMap = 
	List.map (.age >> String.fromInt) users
```

In the example above we have a list of users and two constants<fn-ref id="const" /> that both are a list of the users’ ages as strings. 

The first constant, `agesThroughMaps`, is defined using two map operations. The first run passes the `.age` accessor function to `map` and gives us a list of ages (of type `Int`). In the second map operation the `String.fromInt` is used to create a list of the users’ ages as `String`. 

For the last constant, `agesThroughComposeAndMap`, the list of the users age is created with one map operation where the function passed in to `map` is composed of the `.age` and `fromInt` functions with the compose operator (`>>`). 

And this is the result of the second law: the values in the two constants should be equal. A list of strings: `["20", "22", "30"]`

See full example in Ellie: <https://ellie-app.com/bKhq8M4vyjYa1>

# The consequences of the laws

The laws might seem a bit random and not very helpful at first glance, but it can actually help us a lot. To be a functor the structure/container/context has to follow these laws. It can not do anything weird with the value/values in it or the function that is passed inn. The only thing it can do is apply the function to the value in the context. The functor might have some extra logic around when to apply the function. A list might be empty so there would be nothing to apply the function to and similarly with `Maybe`. Take a look at [last years article](https://functional.christmas/2019/20) to see a more complex example. 

With these rules for what constitutes a functor in place the behavior of functors become predictable. So if we see it in code or talk about it with fellow coders we know exactly how it should behave! 😄

<fn id="accessor">You could use the syntax record.field_name as with JS objects (ex:`user.age`) as well but that does not serve our example that well. :P</fn>
<!-- <fn id="composeop">There is a left compose as well. For our example it would look like: `fromInt << .age` and would create the same function.</fn> -->
<fn id="const">In Elm all data structures are immutable and so are variables/binds so they are really constants. Elm does not support shadowing either.</fn>
