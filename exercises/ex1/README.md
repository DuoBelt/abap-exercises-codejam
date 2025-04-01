# Exercise 1 - Creating your first ABAP classes

In this exercise, we will create a very simple "Hello World" class which outputs to the console.  In the second part of the exercise, we will create another class which SELECTs data from the database and displays the list in the console.   For more information about system access and package creation, please see the [Getting Started](../ex0/README.md) section.

## Exercise 1.1 Creating "Hello World" class

After completing these steps you will have created a "Hello World" ABAP Class.

1.  Right-click on your package and choose “New“, “ABAP Class“.
<br>![](/exercises/ex1/images/01_01_0010.png)

2.  Give the name of the class as "ZCL_HELLO_XXX" where XXX is your group number.  Then click "Next".
<br>![](/exercises/ex1/images/01_01_0020.png)

3.  Click "Finish"
<br>![](/exercises/ex1/images/01_01_0030.png)

4.	In the PUBLIC section of your class,  define the use of the interface IF_OO_ADT_CLASSRUN.  This interface basically gives you access to the console in ADT, which allows the developer to write the results to the console.
<br>![](/exercises/ex1/images/01_01_0040.png)

5.	Next, we need to implement the MAIN method of the inteface.  Enter the code as shown below.  Here the inteface provides access to the "out" instance where we call the "write" method and pass the string "Hello World".
<br>![](/exercises/ex1/images/01_01_0050.png)

6.	Save and activate your class.
<br>![](/exercises/ex1/images/01_01_0060.png)

7.	Next, execute the class by hitting F9.  You should then see "Hello World" in the console tab in ADT.
<br>![](/exercises/ex1/images/01_01_0070.png)

## Exercise 1.2 SELECT data from a database table

After completing these steps you will have created another class which selects data from a database table and outputs the result set to the console.

1.	Use what you have learned and create another class called "ZCL_SELECT_XXX" where XXX is your group number.  In the public section, define the use of interface IF_OO_ADT_CLASSRUN and add the METHOD statements to the implementation section.
<br>![](/exercises/ex1/images/01_02_0010.png)

2.	Next, add the following statements to the PRIVATE section of your class. Here we are defining a table type which contains the structure of the database table which we want to read from.  Also, the METHODS statement defines that your class has a method called "GET_CARRIERS" which returns a list of carriers. 
<br>![](/exercises/ex1/images/01_02_0020.png)

3.	Next, add the implementation of the GET_CARRIERS method as shown.  Here we are doing a SELECT against the database table /DMO/CARRIER and putting results into the RETURNING parameter of the method.
<br>![](/exercises/ex1/images/01_02_0030.png)

4.	Now go to the implementation for the MAIN method and add a statement which calls the GET_CARRIER method and outputs the result set to the console.
<br>![](/exercises/ex1/images/01_02_0040.png)

5.	Save and activate your class
<br>![](/exercises/ex1/images/01_02_0050.png)

6.	Now execute your class by hitting F9.  THe results should be shown in the console.
<br>![](/exercises/ex1/images/01_02_0060.png)

## Summary

You've now created two classes showing how to output results to the console.

Continue to - [Exercise 2 - Exposing and Consuming Services via HTTP ](../ex2/README.md)
Poniżej znajdziesz w pełni sformatowaną wersję tekstu gotową do publikacji na **GitHubie** w pliku `README.md`. Zastosowano **pogrubienia**, **emotikony**, **wypunktowania**, a także poprawne oznaczenie bloków kodu `ABAP` dla lepszej czytelności.

---

# 📘 Ćwiczenia ABAP RAP – Wprowadzenie i Struktura

Ćwiczenia w ramach nauki **ABAP RESTful Application Programming Model (RAP)** to **praktyczny cykl zadań programistycznych**, dzięki którym nauczysz się krok po kroku tworzyć nowoczesne aplikacje biznesowe w środowisku **SAP BTP**.

---

## 🧭 Czym są ćwiczenia ABAP RAP?

W materiałach edukacyjnych ABAP RAP, pojęcie **"ćwiczenia"** odnosi się do praktycznych zadań, które polegają na **tworzeniu i integrowaniu różnych artefaktów programistycznych w systemie ABAP**.

**Celem tych ćwiczeń** jest budowa aplikacji biznesowej udostępnianej w formie usług **OData** (np. dla SAP Fiori), z wykorzystaniem pełnych możliwości chmurowego środowiska ABAP.

W toku realizacji zadań poznajesz m.in. techniki:

- **modelowania danych (CDS)**  
- **definiowania logiki biznesowej (Behavior Definition)**  
- **udostępniania usług (Service Definition & Binding)**  
- **dostrajania interfejsu użytkownika (UI Annotations)**

---

## 🧩 Struktura ćwiczeń krok po kroku

### 1. 🎯 Tworzenie modelu danych (CDS Views)

Pierwsze ćwiczenia koncentrują się na **modelowaniu danych aplikacji**.

**Uczysz się tworzyć** `Core Data Services (CDS)` – podstawowe (`I_*`), projekcyjne (`C_*`) i pomocnicze widoki.

**Przykładowe widoki:**

- `/DMO/I_Connection_R`
- `/DMO/C_TRAVEL_PROCESSOR_M`
- `I_CountryVH`, `I_Airport_StdVH`

**Poznajesz:**

- aliasy (`AS`)  
- adnotacje semantyczne `@Semantics.amount.currencyCode`  
- asocjacje (`association to ...`)  
- użycie `@ObjectModel`

---

### 2. 🧠 Definiowanie zachowania (Behavior Definition & Implementation)

Kolejne ćwiczenia prowadzą przez:

- tworzenie definicji zachowań (`behavior definition`)
- implementację logiki (`behavior implementation`)

**Przykładowe elementy:**

```abap
use action set_status_booked;
use association _BOOKING { create; };
validation check_required_fields on save;
```

**Uczysz się:**

- aktywować akcje biznesowe  
- zarządzać spójnością danych  
- tworzyć reguły walidacyjne  
- definiować efekty uboczne (`side effects`)

---

### 3. 🌐 Tworzenie i publikacja usług (Service Definition & Service Binding)

Tworzysz:

- **Service Definition** – określa, co będzie udostępnione:

```abap
define service Z_TRAVEL_SRV {
  expose /DMO/I_TRAVEL as Travel;
  expose /DMO/I_BOOKING as Booking;
}
```

- **Service Binding** – określa, w jaki sposób usługa ma być udostępniona (np. OData V4)

**Aktywacja bindowania** umożliwia natychmiastowe testowanie aplikacji w przeglądarce **Fiori Elements**.

---

### 4. 🔍 Projekcje i dostrajanie logiki aplikacji (Projections)

Projekcje CDS służą do **dostosowywania danych** do konkretnych przypadków użycia.

**Używane do:**

- filtracji pól  
- przypisywania adnotacji `@UI`  
- przekierowań kompozycji:

```abap
redirected to composition child _BOOKING;
```

**Przykładowe projekcje:**

- `/DMO/C_BOOKING_PROCESSOR_M`  
- `/DMO/C_TRAVEL_APPROVER_M`

---

### 5. 🖼️ Adnotacje interfejsu (UI Annotations)

W ćwiczeniach uczysz się używać adnotacji `@UI` do określania wyglądu aplikacji Fiori:

**Etykiety i pozycjonowanie:**

```abap
@UI.lineItem: [{ position: 10 }]
```

**Wsparcie wyszukiwania:**

```abap
@Search.defaultSearchElement: true
@Search.searchable: true
```

---

### 6. ✈️ Scenariusze biznesowe (Business Scenarios)

Ćwiczenia oparte są na **praktycznych modelach**, np.:

- **Travel Scenario** – zarządzanie podróżami  
- **Flight Reference Model** – zarządzanie rezerwacjami lotów  

**Poznajesz typowe procesy biznesowe** i sposób ich odwzorowania w modelu RAP.

---

### 7. 🧪 Testowanie (Testing)

Wybrane ćwiczenia uczą tworzenia **testów jednostkowych w ABAP**.

**Klasy testowe:**

```abap
CLASS ztest_travel DEFINITION FINAL FOR TESTING.
```

**Poznasz narzędzia:**

- RAP BO Test Double Framework  
- testowanie logiki `Behavior Implementation`  
- testowanie usług OData

---

### 8. 🧱 Rozszerzalność (Extensibility)

W niektórych ćwiczeniach nauczysz się:

- jak **rozszerzyć istniejący model danych lub zachowania**  
- jak **dodać pola niestandardowe (custom fields)**  
- jak **zachować zgodność z modelem extensibility SAP**

---

### 9. 🔗 Konsumpcja usług (Service Consumption)

Ćwiczenia mogą obejmować również:

- tworzenie **klientów OData**  
- testowanie usług **REST z poziomu ABAP**  
- komunikację z **zewnętrznymi interfejsami API**

---

## ✅ Podsumowanie

Każde ćwiczenie to krok na drodze do opanowania nowoczesnego podejścia do programowania aplikacji biznesowych z wykorzystaniem **ABAP RAP**.

W toku nauki:

- **modelujesz dane** przy użyciu **CDS**  
- **definiujesz logikę aplikacyjną** z pomocą **Behavior Definition Language (BDL)**  
- **udostępniasz aplikacje jako usługi OData**  
- **tworzysz kompletne, rozszerzalne aplikacje Fiori** w modelu **Cloud Ready**

