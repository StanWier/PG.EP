Oczyszczone dane, zestandaryzowaną macierz odległości oraz stwierdzam co następuje:

Dane zawierają:
- station_id - wewnętrzne oznaczenie MEVO danej stacji
- name - nazwa stacji, np. GDA370
- address - adres stacji, często bardzo lakoniczny np. Lawendowe wzgórze
- lat, lon - współrzędne geograficzne
- is_virtual_station - True/False ale nie wiem co to znaczy, było w jsonie z którego brałem współrzędne to zostawiłem
- capacity - pojemność stacji, UWAGA: jedna ze stacji miała pojemność 1000, co pewnie jest błędem ale mają tak nawet na apce, chodzi o stację GDA268 - Wełniarska 19/20, jeśli znacie jej pojemność to dajcie znać, na razie zamieniam na "10" - tak jest najczęściej
- points_of_interests - interesujące punkty w promieniu 100m, rozumiane jako:
"school"
"university",
 "kindergarten",
  "hospital",
  "clinic",
  "pharmacy",
  "place_of_worship",
  "library",
   "restaurant",
  "food_court",
  "theatre",
   "cinema",
    "park"
 "museum"

- public_tansport - liczba stacji transportu publicznego w promieniu 100m
- nearest_beach - nazwa najbliższej plaży
- distance_to_beach_m - dystans do najbliższej plaży w metrach
- rent_num - ilość wypożyczonych rowerów w badanym okresie (2024, 2025)
- depo_num - ilość oddanych rowerów w badanym okresie (2024, 2025)
- - morning_rent_num - ilość wypożyczonych rowerów w godz. 6-12
afternoon_rent_num - ilość wypożyczonych rowerów w godz. 12-16
- evening_rent_num - ilość wypożyczonych rowerów w godz. 16-20
- night_rent_num - ilość wypożyczonych rowerów w godz. 20-24
- bilans - depo_num - rent_num

Jest 8 stacji których mi nie wyłapało:
'GDA128',
 'GDA269',
 'GDA272',
 'GDA343',
 'GDY107',
 'GDY110',
 'Opener Kosakowo',
 'WLA011'
Ale jak szukam tych sacji w google to nie mogę znaleźć więc imo olać