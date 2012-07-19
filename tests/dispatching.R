# Pattern matching
# Simple dispatching
a1(0,1) %as%
{

}

a1(x,0) %as%
{
}

a1(x,'green') %as% { 5 }

a1(x,y) %as% { x + y }

# Guards
