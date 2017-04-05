order(pelecaniformes).

family(pelecanidae).
family(ardeidae).
family(threskiornithidae).

genus(pelecanus).
genus(botaurus).
genus(ixobrychus).
genus(ardea).
genus(egretta).
genus(bubulus).
genus(butorides).
genus(nycricorax).
genus(nyctanassa).
genus(eudocimus).
genus(plegadis).
genus(platalea).

species(erythrorhynchos).
species(occidentalis).
species(lentiginosus).
species(exilis).
species(herodias).
species(alba).
species(thula).
species(caerulea).
species(tricolor).
species(rufescens).
species(ibis).
species(virescens).
species(nycticorax).
species(violacea).
species(albus).
species(falcinellus).
species(chihi).
species(ajaja).

hasParent(pelecanidae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(threskiornithidae, pelecaniformes).
hasParent(pelecanus, pelecanidae).
hasParent(botaurus, ardeidae).
hasParent(ixobrychus, ardeidae).
hasParent(ardea, ardeidae).
hasParent(egretta, ardeidae).
hasParent(bubulcus, ardeidae).
hasParent(butorides, ardeidae).
hasParent(nycticorax, ardeidae).
hasParent(nyctanassa, ardeidae).
hasParent(eudocimus, threskiornithidae).
hasParent(plegadis, threskiornithidae).
hasParent(platalea, threskiornithidae).
hasParent(erythrorhynchos, pelecanus).
hasParent(occidentalis, pelecanus).
hasParent(lentiginosus, botaurus).
hasParent(exilis, ixobrychus).
hasParent(herodias, ardea).
hasParent(alba, ardea).
hasParent(thula, egretta).
hasParent(caerulea, egretta).
hasParent(tricolor, egretta).
hasParent(rufescens, egretta).
hasParent(ibis, bubulcus).
hasParent(virescens, butorides).
hasParent(nycticorax, nycticorax).
hasParent(violacea, nyctanassa).
hasParent(albus, eudocimus).
hasParent(falcinellus, plegadis).
hasParent(chihi, plegadis).
hasParent(ajaja, platalea).

hasParent2(nycticorax, ardeidae).
hasParent2(X,Y) :- hasParent(X,Y) *-> \+ species(X).
hasParent2(X,Y) :- hasCompoundName(Y,_,X).

hasCompoundName(pelecanus, erythrorhynchos,pelecanus_erythrorhynchos).
hasCompoundName(pelecanus, occidentalus, pelecanus_occidentalis).
hasCompoundName(botaurus, lentiginosus, botaurus_lentiginosus).
hasCompoundName(ixobrixus, exilis, ixobrychus_exilis).
hasCompoundName(ardea, herodias, ardea_herodias).
hasCompoundName(ardea, alba,ardea_alba).
hasCompoundName(egretta, thula, egretta_thula).
hasCompoundName(egretta, caerula, egretta_caerula).
hasCompoundName(egretta, tricolor, egretta_tricolor).
hasCompoundName(egretta, rufuscens, egretta_rufescens).
hasCompoundName(bubulcus, ibis, bubulcus_ibis).
hasCompoundName(butorides, virescens, butorides_virescens).
hasCompoundName(nycticorax, nycticorax, nycticorax_nycticorax).
hasCompoundName(nyctanassa, violacea, nyctanssa_violacea).
hasCompoundName(eudocimus, albus, eudocimus_albus).
hasCompoundName(plegadis, falcinellus, plegadis_falcinellus).
hasCompoundName(plegadis, chihi, plegadis_chihi).
hasCompoundName(platalea, ajaja, platalea_ajaja).

hasCommonName(pelecanus, pelican).
hasCommonName(pelecanus_erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus_occidentalis, brownPelican).
hasCommonName(botaurus, bittern).
hasCommonName(botaurus_lentiginosus, americanBittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ixobrychus_exilis, leastBittern).
hasCommonName(ardea, heron).
hasCommonName(ardea_herodias, greatBlueHeron).
hasCommonName(ardea_alba, greatEgret).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(egretta_thula, snowyEgret).
hasCommonName(egretta_caerulea, littleBlueHeron).
hasCommonName(egretta_tricolor, tricoloredHeron).
hasCommonName(egretta_rufescens, reddishEgret).
hasCommonName(bubulcus, egret).
hasCommonName(bubulcus_ibis, cattleEgret).
hasCommonName(butorides, heron).
hasCommonName(butorides_virescens, greenHeron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nycticorax_nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(nyctanassa_violacea, yelloCrownedNightHeron).
hasCommonName(eudocimus,ibis).
hasCommonName(eudocimus_albus, whiteIbis).
hasCommonName(plegadis, ibis).
hasCommonName(plegadis_falcinellus, glossyIbis).
hasCommonName(plegadis_chihi, whiteFacedIbis).
hasCommonName(platalea, spoonbill).
hasCommonName(platalea_ajaja, roseateSpoonbill).

hasCommonName(G,S,C) :- hasCompoundName(G,S,Compound), hasCommonName(Compound,C).

hasSciName(C,N) :- hasCommonName(N,C).

isaStrict(A,B) :- hasParent2(A,B).
isaStrict(A,B) :- hasParent2(A,C), isaStrict(C,B).

isa(A,B) :- isaStrict(A,B);(hasCommonName(X,A), isaStrict(X,B)).

synonym(A,B) :- hasCommonName(A,B).
synonym(A,B) :- hasSciName(A,B).
synonym(A,B) :- hasSciName(A,X), hasSciName(B,X), A \= B, !.

rangesTo(pelecanus_erythrorhynchos, canada).
rangesTo(botaurus_lentiginosus, canada).
rangesTo(ardea_herodias, canada).
rangesTo(nycticorax_nycticorax, canada).

habitat(pelecanus_erythrorynchos, marsh).
habitat(pelecanus_occidentalus, ocean).
habitat(botaurus_lentiginosus, marsh).
habitat(ardea_herodias, marsh).
habitat(ardea_herodias, lakePond).
habitat(ardea_herodias, ocean).
habitat(ardea_alba, ocean).
habitat(egretta_thula, ocean).
habitat(egretta_thula, marsh).

nesting(pelecanus_erythrorhynchos, Ground).
nesting(pelecanus_occidentalis, Tree).
nesting(botaurus_lentiginosus, Ground).
nesting(ixobrychus_exilis, Ground).
nesting(ardea_herodias, Tree).
nesting(ardea_alba, Tree).
nesting(egretta_thula, Tree).
nesting(egretta_caerulea, Tree).
nesting(egretta_tricolor, Tree).
nesting(egretta_rufescens, Tree).
nesting(bubulcus_ibis, Tree).
nesting(butorides_virescens, Tree).
nesting(nycticorax_nycticorax, Tree).
nesting(nyctanassa_violacea, Tree).
nesting(eudocimus_albus, Tree).
nesting(plegadis_falcinellus, Ground).
nesting(plegadis_chihi, Ground).
nesting(platalea_ajaja, Tree).

food(pelecanus_erythrorhynchos, Fish).
food(pelecanus_occidentalis, Fish).
food(botaurus_lentiginosus, Fish).
food(ixobrychus_exilis, Fish).
food(ardea_herodias, Fish).
food(ardea_alba, Fish).
food(egretta_thula, Fish).
food(egretta_caerulea, Fish).
food(egretta_tricolor, Fish).
food(egretta_rufescens, Fish).
food(bubulcus_ibis, Insects).
food(butorides_virescens, Fish).
food(nycticorax_nycticorax, Fish).
food(nyctanassa_violacea,  Insects).
food(eudocimus_albus, Insects).
food(plegadis_falcinellus, Insects).
food(plegadis_chihi, Insects).
food(platalea_ajaja, Fish).

conservation(egretta_rufescens, nt).
conservation(X, lc) :- hasCompoundName(_,_,X).
