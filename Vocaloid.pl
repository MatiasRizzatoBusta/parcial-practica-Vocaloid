%vocaloid(nombre,cancion(cancionQueSabe,MinutosQueDura)) %uso functores para practicarlos
vocaloid(megurineLuka,cancion(nightFever,4)).
vocaloid(megurineLuka,cancion(foreverYoung,5)).
vocaloid(hatsuneMiku,cancion(tellYourWorld,4)).
vocaloid(gumi,cancion(foreverYoung,4)).
vocaloid(gumi,cancion(tellYourWorld,5)).
vocaloid(seeU,cancion(novemberRain,6)).
vocaloid(seeU,cancion(nightFever,6)).
%kaito no sabe ninguna asi que ni lo agrego.

%forma 2: vocaloid(nombre,cancion,tiempoQueDura).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Punto 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODOS LOS PREDICADOS  DEBEN SER INVERSIBLES

esNovedoso(Vocaloid):-
    vocaloid(Vocaloid,cancion(Cancion,_)),
    vocaloid(Vocaloid,cancion(OtraCancion,_)),
    Cancion \= OtraCancion, %de esta forma me aseguro que sepa minimo 2
    sumaDuracionTemas(Vocaloid,DuracionTotal),
    DuracionTotal < 15.

sumaDuracionTemas(Vocaloid,DuracionTotal):-
    vocaloid(Vocaloid,_),
    findall(Duracion,vocaloid(Vocaloid,cancion(_,Duracion)),ListaDuracionesTemasSabidos),%busco cuando duran todos los temas que sabe
    sumlist(ListaDuracionesTemasSabidos, DuracionTotal).
    
esAcelerado(Vocaloid):- %no usar forall
    vocaloid(Vocaloid,cancion(_,Duracion)),
    not(Duracion > 4).
    


