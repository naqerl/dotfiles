#!/bin/bash
echo 'digraph all_deps {' > all_deps.dot
echo '  graph [nodesep=".1",rankdir="LR",ranksep=".1",ratio="compress",splines="polyline",label="[XBPS] All user-installed packages dependency graph"];' >> all_deps.dot
echo '  edge [arrowhead="vee",arrowsize=".4",constraint="true",fontname="Sans",fontsize="8"];' >> all_deps.dot
echo '  node [fontname="Sans",fontsize="8",height=".1",shape="ellipse",width=".1"];' >> all_deps.dot
for pkg in $(xbps-query -m); do
  xbps-dgraph -f "$pkg" >> all_deps.dot
done
echo '}' >> all_deps.dot
