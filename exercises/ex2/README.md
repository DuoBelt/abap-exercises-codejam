# Exercise 2 - Exposing and Consuming Services via HTTP

In this exercise, we will create an HTTP serivce and custom ABAP HTTP handler class.  We will then consume an external HTTP service as well by leveraging the SAP API Hub.   You can view APIs of the SAP API Hub at the following URL.

https://api.sap.com/package/SAPS4HANACloud/all

In this exercise, we will use the Bank – Read API, which allows us to retrieve a list of bank details.  This page gives you all the information about calling the API and also supplies the API key which we will need to supply when calling from our ABAP class.

https://api.sap.com/api/API_BANKDETAIL_SRV/resource

## Exercise 2.1 Expose an HTTP Service in ABAP

After completing these steps you will have created an HTTP service along with its handler class. 

1. Right-click on your package and choose “New“, “Other ABAP Repository Object“.
<br>![](/exercises/ex2/images/02_01_0010.png)

2. Under "Connectivity, choose "HTTP Service" and click "Next".
<br>![](/exercises/ex2/images/02_01_0020.png)

3. Give the name of the service as ZHTTPSRV_XXX where XXX is your group number.  Keep the handler class name as generated, then click "Next".
<br>![](/exercises/ex2/images/02_01_0030.png)

4. Click "Finish".
<br>![](/exercises/ex2/images/02_01_0040.png)

5. Your HTTP Service is now created, click on the "Handler Class" link, this will take you to that class so that you can implement it. 
<br>![](/exercises/ex2/images/02_01_0050.png)

6. Here you can see that the correct interface has been added to the PUBLIC section for you, and the HANDLE_REQUEST method shell is there in the implementation as well. 
<br>![](/exercises/ex2/images/02_01_0060.png)

7. Next, add the following code to the HANDLE_REQUEST method.  In this method, we will retrieve the URL parameters from the request object and check for the "cmd" parameter and get its value.  Based on that value, we will do something, for example, if cmd=timestamp, then we will write the curret timestamp to the response object. 
```abap
   DATA(lt_params) = request->get_form_fields(  ).
    READ TABLE lt_params REFERENCE INTO DATA(lr_params) WITH KEY name = 'cmd'.
    IF sy-subrc <> 0.
      response->set_status( i_code = 400
                       i_reason = 'Bad request' ).
      RETURN.
    ENDIF.
    CASE lr_params->value.
      WHEN `timestamp`.        
        response->set_text( |HTTP Service Application executed by {
                             cl_abap_context_info=>get_user_technical_name( ) } | &&
                              | on { cl_abap_context_info=>get_system_date( ) DATE = ENVIRONMENT } | &&
                              | at { cl_abap_context_info=>get_system_time( ) TIME = ENVIRONMENT } | ).

      WHEN OTHERS.
        response->set_status( i_code = 400 i_reason = 'Bad request' ).
    ENDCASE.
```

8. Your code should now look like this.
<br>![](/exercises/ex2/images/02_01_0070.png)

9. Save and activate your class.
<br>![](/exercises/ex2/images/02_01_0080.png)

10. Now return to the HTTP Service and click on the "URL" link.
<br>![](/exercises/ex2/images/02_01_0090.png)

11. The browser should open and you may have to log on. Once you are logged on, you will most likely get this screen. We are getting an error because we have not set the URL parameter called "cmd" yet. We will do this next. 
<br>![](/exercises/ex2/images/02_01_0100.png)

12. Add &cmd=timestamp to the end of the URL in the browser and hit enter. 
<br>![](/exercises/ex2/images/02_01_0110.png)

13. You should now see the correct output.
<br>![](/exercises/ex2/images/02_01_0120.png)


## Exercise 2.2 Consume an external HTTP service in ABAP

After completing these steps you will have consumed an external API from the SAP API Hub, and viewed the results via the browser.

1. First, lets go to the SAP API Hub. From the browser, go to https://api.sap.com/api/API_BANKDETAIL_SRV/resource.  Log on to the site, you may have to register first. <br>![](/exercises/ex2/images/02_02_0010.png)

2. Click the "Show API Key" button.
<br>![](/exercises/ex2/images/02_02_0020.png)

3. Next, click the "Copy Key and Close" button.  Paste this code somewhere in a safe place, we will need it a little later. 
<br>![](/exercises/ex2/images/02_02_0030.png)

4.	Now return to your ZCL_HTTPSRV_XXX class and modify it.  Add a METHOD called get_bank_details with a returning parameter of type string to the private section. Also add the shell of the METHOD implementation as well.  
<br>![](/exercises/ex2/images/02_02_0040.png)

5. Add the implementation of the GET_BANK_DETAILS method as shown below.  Here we are utlizing the SAP delivered HTTP interface classes.  We access the request object and insert the API key and the execute the request and retrieve the result.  We then pass the result to the output parameter as JSON.
```abap

    DATA: lv_url TYPE string VALUE 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/'.
    DATA: lo_http_client TYPE REF TO  if_web_http_client.

    lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( lv_url ) ).

    DATA(lo_request) = lo_http_client->get_http_request( ).

    lo_request->set_header_fields( VALUE #(
       (  name = 'Content-Type' value = 'application/json' )
       (  name = 'Accept' value = 'application/json' )
       (  name = 'APIKey' value = '<insert API key here>') ) ).  "<<-- API KEY!

    lo_request->set_uri_path(
       i_uri_path = lv_url && 'API_BANKDETAIL_SRV/A_BankDetail?$top=25&$format=json'  ).

    TRY.
        DATA(lv_response) = lo_http_client->execute( i_method = if_web_http_client=>get )->get_text(  ).
      CATCH cx_web_http_client_error.
    ENDTRY.

    r_json = lv_response.

```
6.	Your code should now look like this.
<br>![](/exercises/ex2/images/02_02_0050.png)

7.	Next, go to the HANDLE_REQUEST method, and modify it.  Add another WHEN condition in the CASE statement.  When the URL parameter cmd is = getbankdetails, set the content type to 'application/json' and call the GET_BANK_DETAILS method and pass the returning string to the response object SET_TEXT method. 
<br>![](/exercises/ex2/images/02_02_0060.png)

8.	Save and activate your work.
<br>![](/exercises/ex2/images/02_02_0070.png)

9.	Once again return to the HTTP service and click the "URL" link.  Add the URL parameter as &cmd=getbankdetails and hit enter.  You should now see the bank details in JSON format. 
<br>![](/exercises/ex2/images/02_02_0080.png)

5.	CHALLENGE!  Use what you have learned and implement a new method in your ZCL_HTTPSRV_XXX class for another API.  You can choose one from this page. https://api.sap.com/package/SAPS4HANACloud/all. Choose one that is an OData API and perhaps one that is a READ type operation

6. As your API keys are now exposed in your code, I would highly recommend that you return to your ZCL_MY_HTTPSRV_XXX class and remove them.


## Summary

Poniżej znajdziesz **sformatowaną instrukcję krok po kroku** do ćwiczenia **„Ćwiczenie 2 – Uwidacznianie i Konsumowanie Usług przez HTTP”**, gotową do publikacji w pliku `README.md` repozytorium GitHub. Zastosowano nagłówki, listy, pogrubienia, bloki kodu `ABAP`, objaśnienia pojęć i sekcję wyzwania dla uczestników:

---

# 🌐 Ćwiczenie 2 – Uwidacznianie i Konsumowanie Usług przez HTTP

> To ćwiczenie pokaże Ci, jak **tworzyć własne usługi HTTP w systemie ABAP** oraz jak **konsumować zewnętrzne API** z wykorzystaniem SAP API Hub.  

---

## 🧩 Struktura ćwiczenia

Ćwiczenie składa się z dwóch części:

1. **Uwidacznianie własnej usługi HTTP w ABAP**
2. **Konsumpcja zewnętrznej usługi HTTP z SAP API Hub**

---

## 📘 Część 2.1 – Uwidacznianie Usługi HTTP w ABAP

### ✅ Krok 1 – Utworzenie usługi HTTP

1. Kliknij **prawym przyciskiem** na swój pakiet w ADT.  
2. Wybierz: `New → Other ABAP Repository Object`  
3. Wybierz: `Connectivity → HTTP Service → Next`  
4. Nadaj nazwę usłudze, np. `ZHTTPSRV_XXX`  
5. Kliknij `Finish` – system utworzy klasę handlera.

---

### ✅ Krok 2 – Implementacja klasy obsługi HTTP

1. Przejdź do klasy handlera (`Handler Class`)  
2. Znajdziesz metodę: `IF_HTTP_EXTENSION~HANDLE_REQUEST`  
3. Dodaj poniższy kod:

```abap
METHOD if_http_extension~handle_request.

  DATA: lv_timestamp TYPE timestampl.

  GET TIME STAMP lv_timestamp.

  DATA(lv_response) = |Timestamp: { lv_timestamp }|.

  response->set_text( lv_response ).
  response->set_status( cl_http_status=>http_ok, 'OK' ).
  response->set_header_field( name  = 'Content-Type'
                              value = 'text/plain' ).

ENDMETHOD.
```

4. Zapisz i aktywuj klasę.

---

### ✅ Krok 3 – Testowanie usługi

1. Wróć do obiektu `HTTP Service`  
2. Kliknij link `URL`  
3. W przeglądarce dodaj na końcu adresu: `&cmd=timestamp`  
4. Naciśnij `Enter` – powinieneś zobaczyć aktualny znacznik czasu.

---

## 🧠 Słowniczek pojęć

- **Usługa HTTP (HTTP Service):** obiekt w systemie ABAP obsługujący żądania HTTP.  
- **Klasa obsługi HTTP (Handler Class):** klasa implementująca `IF_HTTP_EXTENSION`, zawiera metodę `HANDLE_REQUEST`.  
- **Żądanie HTTP:** komunikat od klienta (np. przeglądarki).  
- **Odpowiedź HTTP:** komunikat zwrotny od serwera.  
- **Parametry URL:** dodatkowe dane przekazywane w adresie URL, np. `&cmd=timestamp`.

---

## 🌍 Część 2.2 – Konsumpcja zewnętrznej usługi HTTP

### ✅ Krok 1 – Uzyskanie API Key z SAP API Hub

1. Przejdź do: [`https://api.sap.com/api/API_BANKDETAIL_SRV/resource`](https://api.sap.com/api/API_BANKDETAIL_SRV/resource)  
2. Zaloguj się i kliknij `Show API Key`  
3. Skopiuj swój klucz – będzie potrzebny w ABAP

---

### ✅ Krok 2 – Implementacja metody `GET_BANK_DETAILS`

1. Dodaj do klasy nową metodę `GET_BANK_DETAILS`:

```abap
METHOD get_bank_details RETURNING r_json TYPE string.

  DATA: lv_url         TYPE string VALUE 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/'.
  DATA: lo_http_client TYPE REF TO if_web_http_client.

  lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( lv_url )
  ).

  DATA(lo_request) = lo_http_client->get_http_request( ).
  lo_request->set_header_fields( VALUE #(
    ( name = 'Content-Type' value = 'application/json' )
    ( name = 'Accept'       value = 'application/json' )
    ( name = 'APIKey'       value = '<TWÓJ_KLUCZ_API>' ) " <--- Wstaw swój klucz!
  ) ).

  lo_request->set_uri_path(
    i_uri_path = lv_url && 'API_BANKDETAIL_SRV/A_BankDetail?$top=25&$format=json'
  ).

  TRY.
    DATA(lv_response) = lo_http_client->execute( i_method = if_web_http_client=>get )->get_text( ).
  CATCH cx_web_http_client_error.
  ENDTRY.

  r_json = lv_response.

ENDMETHOD.
```

---

### ✅ Krok 3 – Modyfikacja `HANDLE_REQUEST`

Dodaj warunek do metody:

```abap
METHOD if_http_extension~handle_request.

  IF request->get_form_field( 'cmd' ) = 'timestamp'.
    DATA(lv_timestamp) TYPE timestampl.
    GET TIME STAMP lv_timestamp.
    response->set_text( |Timestamp: { lv_timestamp }| ).
    response->set_status( cl_http_status=>http_ok, 'OK' ).
    response->set_header_field( name = 'Content-Type' value = 'text/plain' ).

  ELSEIF request->get_form_field( 'cmd' ) = 'bankdetails'.
    DATA(lv_bank_details) = get_bank_details( ).
    response->set_text( lv_bank_details ).
    response->set_status( cl_http_status=>http_ok, 'OK' ).
    response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDIF.

ENDMETHOD.
```

---

### ✅ Krok 4 – Testowanie usługi zewnętrznej

1. Przejdź do adresu URL Twojej usługi  
2. Użyj parametru: `&cmd=bankdetails`  
3. Powinieneś otrzymać dane w formacie JSON z SAP API Hub

---

## 🧠 Słowniczek pojęć (część 2)

- **SAP API Hub:** katalog otwartych interfejsów API SAP  
- **API Key:** identyfikator do uwierzytelniania żądań  
- **HTTP GET:** metoda do pobierania danych  
- **JSON:** format wymiany danych  
- **Klasy HTTP ABAP:** klasy systemowe SAP do pracy z HTTP, m.in.:  
  - `CL_WEB_HTTP_CLIENT_MANAGER`  
  - `IF_WEB_HTTP_CLIENT`  
  - `IF_HTTP_REQUEST`  
  - `IF_HTTP_RESPONSE`

---

## 🎯 Wyzwanie

> Na podstawie zdobytej wiedzy:
>
> 🔧 Dodaj nową metodę do swojej klasy `ZCL_HTTPSRV_XXX`, która konsumuje inne API z SAP API Hub (np. waluty, kraje, jednostki miary).  
>
> 🎯 Cel: przetestuj swój kod, dodaj kolejną wartość `cmd` w URL.

---

## ✅ Podsumowanie

Po wykonaniu ćwiczenia potrafisz:

- Tworzyć i wystawiać własne usługi HTTP w systemie ABAP  
- Implementować klasy `Handler` obsługujące logikę dla usług  
- Konsumować zewnętrzne API (OData/REST) z SAP API Hub  
- Przetwarzać dane JSON w odpowiedzi  
- Przekazywać dane przez URL i reagować na parametry

--



You've now created a new HTTP service that exposes functionality from ABAP as well as you have learned how to consume an external service via HTTP. 

Continue to - [Exercise 3 - Service Consumption Model ](../ex3/README.md)
