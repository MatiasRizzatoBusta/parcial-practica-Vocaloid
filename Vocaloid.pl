
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODOS LOS PREDICADOS  DEBEN SER INVERSIBLES

esNovedoso(Vocaloid):-
    vocaloid(Vocaloid,_),
    cantidadDeTemasQueSabe(Vocaloid,CantidadDeTemasQueSabe),
    CantidadDeTemasQueSabe >= 2,
    sumaDuracionTemas(Vocaloid,DuracionTotal),
    DuracionTotal < 15.

cantidadDeTemasQueSabe(Vocaloid,CantidadDeTemasQueSabe):-
    findall(Cancion,vocaloid(Vocaloid,cancion(Cancion,_)),ListaCanciones),
    length(ListaCanciones,CantidadDeTemasQueSabe).

sumaDuracionTemas(Vocaloid,DuracionTotal):-
    vocaloid(Vocaloid,_),
    findall(Duracion,vocaloid(Vocaloid,cancion(_,Duracion)),ListaDuracionesTemasSabidos),%busco cuando duran todos los temas que sabe
    sumlist(ListaDuracionesTemasSabidos, DuracionTotal).
    
esAcelerado(Vocaloid):- %no usar forall
vocaloid(Vocaloid,_),% esto lo pongo para ligar a una variable para el not.El not trabaja con variables ya ligadas
not((vocaloid(Vocaloid,cancion(_,Duracion)),Duracion < 4)).% que ningun tema dure mas de 4 minutos.

%vocaloid(Vocaloid,_),
%forall(vocaloid(Vocaloid,cancion(_,Duracion)),Duracion =< 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%concierto(nombre,tipo,lugar,fama).
%tipo(gigante(cancionesMinimas,duracionMinimaTotal)).
%tipo(mediano(DuracionTOtalMaxima)).
%tipo(chico(duracionMinimaCancion)).
concierto(mikuExpo,gigante(2,6),estadosUnidos,2000).
concierto(magicalMirai,gigante(3,10),japon,3000).
concierto(vocalektVisions,mediano(9),estadosUnidos,1000).
concierto(mikuFest,chico(4),argentina,100).

puedeParticipar(Vocaloid,Concierto):-
    vocaloid(Vocaloid,_),
    concierto(Concierto,Tipo,_,_),
    cumpleRequisitoConcierto(Vocaloid,Tipo).

puedeParticipar(hatsuneMiku,_). %puede ir a cualquiera.

cumpleRequisitoConcierto(Vocaloid,gigante(CancionesMinimas,TiempoMinimo)):-
    vocaloid(Vocaloid,_),
    cantidadDeTemasQueSabe(Vocaloid,CantidadDeCanciones),
    CantidadDeCanciones >= CancionesMinimas,
    sumaDuracionTemas(Vocaloid,DuracionTotal),
    DuracionTotal >= TiempoMinimo.

cumpleRequisitoConcierto(Vocaloid,mediano(DuracionMaximaTotal)):-
    vocaloid(Vocaloid,_),
    sumaDuracionTemas(Vocaloid,DuracionTotal),
    DuracionTotal < DuracionMaximaTotal.

cumpleRequisitoConcierto(Vocaloid,chico(DuracionMinima)):-
    vocaloid(Vocaloid,cancion(_,Duracion)),
    Duracion > DuracionMinima.

elMasFamoso(Vocaloid):-
    vocaloid(Vocaloid,_),
    famaVocaloid(Vocaloid,FamaTotal),
    forall((vocaloid(OtroVocaloid,_),OtroVocaloid \= Vocaloid,famaVocaloid(OtroVocaloid,OtraFamaTotal)),OtraFamaTotal > FamaTotal).
%tomo la fama de uno y despues la comparo con la de los demas. me quedo con el que mas fama tenga. es hatsune miku pq puede ir
%a todos.

famaVocaloid(Vocaloid,FamaTotal):-
    cantidadDeTemasQueSabe(Vocaloid,CantidadDeTemasQueSabe),
    findall(Fama,(puedeParticipar(Vocaloid,Concierto),concierto(Concierto,_,_,Fama)),ListaFama),
    length(ListaFama,FamaQueConsigue),
    FamaTotal is (FamaQueConsigue * CantidadDeTemasQueSabe).

conoce(megurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).

esElUnicoQueParticipa(Vocaloid,Concierto):-
    conoce(Vocaloid,Conocido),
    puedeParticipar(Vocaloid,Concierto),
    not(puedeParticipar(Conocido,Concierto)),
    esElUnicoQueParticipa(Conocido,Concierto).

/*si tenemos que agregar mas tipos de conciertos solo tenemos que agregar dico concierto y agregar una funcion de 
cumpleCriterioConcierto que se fije si el vocaloid cumple los criteriso de dicho concierto.
El concepto que permite esto es el polimorfismo ya que nos permite tratar de forma diferente cada tipo de concierto.\
*/