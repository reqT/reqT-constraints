# reqT-constraints

This repo is a zero-dependency [Scala](https://www.scala-lang.org/)-embedded [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for expressing [constraint satisfaction problem](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem). It has a permissive license. 

I can be used together with `reqT-lang` (permissive license) and `reqT-jacop` (AGPL-v3).

# Use

If you want to use `scala-cli` with the jacop solver and `reqt-lang` then include these dependencies:

```scala
//> using scala 3.4
//> using dep "org.jacop:jacop:4.10.0"
//> using dep "reqt-constraints:reqt-constraints:1.0.0,url=https://github.com/reqT/reqT-constraints/releases/download/v1.0.0/reqt-constraints_3-1.0.0.jar"
//> using dep "reqt-jacop:reqt-jacop:1.0.0,url=https://github.com/reqT/reqT-jacop/releases/download/v1.0.0/reqt-jacop_3-1.0.0.jar"
//> using dep "reqt-lang:reqt-lang:4.0.0-RC2,url=https://github.com/reqT/reqT-lang/releases/download/4.0.0-RC2/reqt-lang_3-4.0.0-RC2.jar"
```

# Build

`sbt build`

# Contribute

Contributions are welcome! Contact bjorn.regnell@cs.lth.se

# Publish

* `sbt package`

* Upload jar in target/scala-x.y.z to github releases.

* Update this README with version info.


