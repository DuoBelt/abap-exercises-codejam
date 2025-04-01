# Getting Started

In this exercise, you will connect to the system and create your package. Be sure to use your assigned group number anywhere you see XXX in this document. 

## System Access & Package Creation

After completing these steps you will have accessed your trial account on SAP Business Technology Platform, created a service instance for the ABAP Trial on BTP, created a connection to the system from ABAP Development Tools in Eclipse, and finally created your group package which will be used for the rest of your exercise.

1.  Access the BTP, ABAP Environment Trial system by following the directions outlined in this [tutorial](https://developers.sap.com/tutorials/abap-environment-trial-onboarding.html). Once complete, return to this document and continue with step 2.  

2.	Right-click on the “Favorite Packages“ and choose “Add Package“.
<br>![](/exercises/ex0/images/00_00_0020.png)

3.	Search for ZLOCAL and click “OK“.
<br>![](/exercises/ex0/images/00_00_0030.png)

4.	Under the “Favorite Packages” folder, expand the ZLOCAL folder.
<br>![](/exercises/ex0/images/00_00_0040.png)

5.	Right-click on the ZLOCAL package and choose “New“, then “ABAP Package“. 
<br>![](/exercises/ex0/images/00_00_0050.png)

6.	Give the name of the package as Z_GRP_XXX where XXX is your group number. Group numbers will be assigned by the instructor. Also give a meaningful description and click “Next“.
<br>![](/exercises/ex0/images/00_00_0060.png)

7.	Click “Next”.
<br>![](/exercises/ex0/images/00_00_0070.png)

8.	If you do not currently have a transport request in this system, select the radiobutton to create one and give a description.  Click “Finish”.
<br>![](/exercises/ex0/images/00_00_0080.png)


## Summary

Now that you have connected to the system, logged on, and created your package, you are ready to start building your application.  
Continue to - [Exercise 1 - Creating your first ABAP classes](../ex1/README.md)

Oczywiście! Oto przygotowana i sformatowana wersja treści na potrzeby publikacji w pliku `README.md` na GitHubie – z zachowaniem odpowiednich znaczników Markdown, logiczną strukturą nagłówków i formatowaniem sprzyjającym czytelności:

---

# Ćwiczenie Zerowe – Dostęp do Systemu i Tworzenie Pakietu

> Szczegółowe wyjaśnienie kroków przygotowawczych w środowisku **SAP BTP ABAP Environment Trial**.

---

## Wprowadzenie

To ćwiczenie stanowi fundament dalszej pracy i pozwala Ci przygotować środowisko programistyczne. Poniżej znajdziesz rozszerzone wyjaśnienia dotyczące każdego kroku.

---

## 1. Dostęp do systemu BTP, ABAP Environment Trial

### SAP Business Technology Platform (BTP)

SAP BTP to **platforma jako usługa (PaaS)**, która umożliwia tworzenie, integrowanie i rozszerzanie aplikacji SAP. Udostępnia m.in. środowiska programistyczne, usługi baz danych i narzędzia analityczne.

### ABAP Environment Trial

Środowisko próbne ABAP w BTP umożliwia testowanie najnowszych rozwiązań ABAP, w tym **ABAP RESTful Application Programming Model (RAP)**. Dostępne jest bezpłatnie i idealne do nauki oraz eksperymentowania.

### Znaczenie dostępu

Bez dostępu do systemu nie można:
- tworzyć obiektów ABAP,
- testować kodu,
- realizować kolejnych ćwiczeń.

Instrukcja konfiguracji znajduje się w osobnym tutorialu – to kluczowy, ale osobno opisany etap.

---

## 2. Dodanie pakietu lokalnego `ZLOCAL` do ulubionych

### Czym jest pakiet ABAP?

Pakiet grupuje powiązane ze sobą obiekty ABAP (klasy, CDS, programy, itp.), umożliwiając:
- lepszą organizację,
- kontrolę zależności,
- zarządzanie dostępem.

### Pakiety `$TMP` i `ZLOCAL`

- `$TMP` – tymczasowy, lokalny, nie służy do transportu.
- `ZLOCAL` – również lokalny, ale używany w środowiskach szkoleniowych/demonstracyjnych.

### Dlaczego dodać `ZLOCAL` do ulubionych?

Ułatwia szybki dostęp w **ABAP Development Tools (ADT)** – nie trzeba za każdym razem wyszukiwać pakietu ręcznie.

---

## 3. Utworzenie nowego pakietu ABAP w `ZLOCAL`

### Tworzenie własnego pakietu `Z_GRP_XXX`

Przykład: `Z_GRP_003` – pakiet konkretnej grupy. Dzięki temu:
- obiekty są odseparowane od innych,
- łatwiej nimi zarządzać,
- można później je łatwo zidentyfikować.

### Narzędzie: ABAP Development Tools (ADT)

Pakiet tworzony jest w środowisku **Eclipse + ADT**, zintegrowanym z SAP BTP.

---

## 4. Nadanie nazwy i opisu nowemu pakietowi

### Konwencja nazewnictwa

- `Z_` – prefiks dla obiektów niestandardowych.
- `GRP` – oznaczenie grupy.
- `XXX` – numer Twojej grupy.

**Przykład:** `Z_GRP_012`

### Znaczący opis

Opis pakietu powinien jasno wskazywać jego zawartość i cel, np.:
```
Obiekty ćwiczeń grupy 12
Pierwsza aplikacja RAP – grupa 7
```

---

## 5. Wybór lub utworzenie Transport Request

### Czym jest Transport Request?

To mechanizm SAP do rejestrowania zmian obiektów w systemie, które mają być transportowane między środowiskami (np. DEV → QA → PROD).

### Transport w kontekście ćwiczeń

Mimo że środowisko ABAP Trial nie wymaga rzeczywistego transportu, system nadal oczekuje przypisania obiektów do transportu – nawet lokalnego.

**Dobre praktyki:**
- Twórz własny request.
- Używaj opisów typu: `Obiekty ćwiczeń – grupa 003`.

---

## Podsumowanie

> Ćwiczenie zerowe przygotowuje Twoje środowisko i strukturę pracy z kodem ABAP w chmurze SAP BTP.

Poprawna konfiguracja na tym etapie:
- chroni przed problemami technicznymi,
- ułatwia pracę z obiektami ABAP,
- umożliwia płynne przejście do tworzenia aplikacji z użyciem ABAP RESTful.

---

