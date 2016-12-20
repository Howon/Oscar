#include <oscar/immut.hpp>

int main () 
{
auto pong=actor<a_Pong>();
auto ping=actor<a_Ping>(pong);
m_setMaxTurns(3) |> ping;
m_startMsg() |> ping;
return 0;
}

