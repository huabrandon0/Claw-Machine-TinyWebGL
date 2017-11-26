CS 174A Project: An Arcade Claw Machine
Brandon Hua

For this project, I made a arcade claw machine game. The following are its core
functions: the ability to control the claw's position, the ability to trigger
the claw to descend and grip, and the claw's abilities to grab a prize (or
not!), return to origin, and drop the prize. The game plays like a typical claw
machine, but there are unlimited plays and the items can be reset. When the
player wins all the prizes, a victory sound plays.

Custom shape:
The custom shape I made was a diamond (just two square pyramids put together).
In the scene, the diamonds are teal-colored and mapped to a crystal-like
texture.

Usage of look_at():
The look_at() function is always being used differently depending on the state
of the animation. For instance, in default state, the camera follows a point
that is ~20 units below the claw. But in "descending" state, the camera changes
position to be closer to the machine and follows a point that is ~5 units below
the claw.

Other notes:
A "hierarchical object with at least three levels" technically doesn't exist
until a prize is picked up. The event of the claw picking up a prize causes an
object of three hierarchical levels to be created:
  claw (highest) -> claw's carrying position -> prize being carried (lowest)

There were some mentions of extra credit for implementing physics during class,
but there was no official announcement it seems. In the case that there is
extra credit, note that the black wire attached to the claw is implemented
using spring physics.
