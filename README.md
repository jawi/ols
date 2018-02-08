# OpenBench LogicSniffer

![OpenBench LogicSniffer](http://s1.directupload.net/images/140913/bca9sg2g.png "Screenshot of ols")

This is the public [Git][git] repository for the OpenBench LogicSniffer (*ols*
for short) written by J.W. Janssen. It is an alternative client for an open
source [logic analyzer](https://en.wikipedia.org/wiki/Logic_analyzer) called
the [Open Bench Logic Sniffer](http://dangerousprototypes.com/docs/Open_Bench_Logic_Sniffer).
Refer to this [page](https://lxtreme.nl/projects/ols/) for more information about the
project.

## FEATURES

The alternative Java client provides the following features:

- Cross platform: The client runs on Mac OS X (32/64-bit), Windows (32/64 bit),
  Linux (32/64 bit) and Solaris (32 bit)

- Simple installation: No longer fiddling with the serial libraries (RXTX) in
  order to get the client up and running. The client embeds the suitable serial
  libraries for several operating systems

- Pluggable: Adding new functionality on the fly is possible and as easy as
  copying files to a single directory

- Looks and feels good: The client has a good look and feel, aiming at being as
  usable as possible and adhering to the human interface guidelines of the
  platform it is running on.

## DOCUMENTATION

All documentation is maintained in the [wiki](https://github.com/jawi/ols/wiki).

## COMPILING THE SOURCES

In case you are interested in cloning this repository and compile it for
yourself, you should do the following:

  $ git clone http://github.com/jawi/ols.git

For compiling the sources, you need to have at least a valid JDK (1.6+) and
[Maven][maven] installed. For developing, I recommend
[Eclipse][eclipse] as development environment.

  $ cd ols/
  $ mvn clean install

After this, you should find the latest binary ZIP or tarball in
`ols.distribution/target`.

## DEVELOPING FOR OLS

Developing for ols can be done with any *modern* IDE, like [Eclipse][eclipse],
[Netbeans][netbeans], or even [Emacs][emacs]. Keep in mind that IDE-specific
stuff is not committed to the repository as this would clutter it unnecessarily.

Keep in mind that your IDE should provide support for [Maven][maven], otherwise
it most probably will not compile out of the box. In addition, you probably
want support for [Git][git] in your IDE as well. To create the needed project
files for [Eclipse][eclipse], for example, you can use the following
[Maven][maven] command:

  $ mvn eclipse:eclipse

There are similar commands for other IDEs. See the [Maven][maven] site for more
details on this.

Some notes for [Eclipe][eclipse]: After having imported the projects into your
workspace, you might need to enable [Maven][maven] support by hand. Simply
select all projects, right click on them and choose
`Enable Dependency Management` from the [Maven][maven] menu should be
sufficient. For running the OLS client in [Eclipse][eclipse], you can make use
of the launch configurations found in the `eclipse/` subdirectory of the OLS
repository. The code formatting rules and cleanup rules can be found there, too.

## CONTRIBUTIONS

Drop me a line if you want to contribute code to the OLS repository. If needed
I can give you write-access to the GitHub or apply your patch directly.

## CONTACT

You can reach me at: `j dot w dot janssen at lxtreme.nl`

## DONATIONS

You can support and encourage further development of this project through the
following means:

[![Flattr This!](http://api.flattr.com/button/flattr-badge-large.png "Flattr This!")](https://flattr.com/thing/61272/OpenBench-LogicSniffer-alternative-Java-Client)

[![Bountysource](https://d2bbtvgnhux6eq.cloudfront.net/assets/Bountysource-green-712770df4397a3bc6f5b56b90402763c.png "Bountysource logo")](https://www.bountysource.com/trackers/315759-jawi-ols)

## LICENSE

[![GNU GPLv2](https://www.gnu.org/graphics/heckert_gnu.small.png "GNU GPLv2")](https://www.gnu.org/licenses/gpl-2.0.html)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

[eclipse]: https://www.eclipse.org/
[maven]: https://maven.apache.org/
[netbeans]: https://netbeans.org/
[emacs]: https://www.gnu.org/software/emacs/
[git]: http://git-scm.com/

