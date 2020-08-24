## Convexhull Algorithm list

Below list is convexhull algorithm names and time complexity inside parenthesis.
In time complexity, "n" is the number of points and h is the number of poitns on convexhull.

| Algorithm Name                    | Time complexity | Implementation |
|:----------------------------------|:---------------:|:--------------:|
| Gift wrapping                     | O(nh)           | O              |
| Graham scan                       | O(nlog(n))      | X              |
| Quick hull                        | O(nlog(n))      | X              |
| Divide and conquer                | O(nlog(n))      | X              |
| Monotone chain                    | O(nlog(n))      | X              |
| Incremental convex hull algorithm | O(nlog(n))      | X              |
| Kirkpatrick-Sedel algorithm       | O(nlog(n))      | X              |
| Chan's algorithm                  | O(nlog(n))      | X              |

## Specification

- **Benchmark** : Not to be determined
- **Test Framework** : [Hspec](http://hspec.github.io/)

## Usage 

```Haskell
import Convexhull
import Geos

pts = [Point 2 4, Point 5 13, Point 4 3, Point 13 5, Point 0 0, Point 8 4, Point 10 10, Point 3 2, Point 10 0, Point 4 9, Point 0 10]

convexPts = giftWrapping pts
-- [Point 0 0, Point 0 10, Point 5 13, Point 10 10, Point 13 5, Point 10 0]

```
