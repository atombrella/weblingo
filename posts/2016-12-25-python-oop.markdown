---
title: Python OOP Guidelines
keywords: python, oop
description: quick overview of the oop-principles in python for non-python people
---

If you come from another language, switching to Python may or not come easily.
There are a lot of features that are not present in, e.g., Java.

Python includes elements of functional programming (map, lambda, reduce/fold),
context managers, decorators and lots of other nice things which are useful for
writing nice code.  Unlike Java, there are no explicit way of writing private or
protected methods, and thus, the convention is to do it by prefixing the method
name with an underscore to indicate that it's private.

Decorators are powerful, similar to annotations in Java. Decorators can be used
to add additional methods to your class, e.g., [total_ordering](https://)



``` python
@total_ordering
class Dinosaur(object):
    def __init__()

```
