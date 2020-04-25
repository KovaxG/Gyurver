module Articles exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h2, h3, p, a, br, strong)
import Html.Attributes exposing (href)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup

type Model = NoModel
type Msg = NoMsg
  
init : (Model, Cmd Msg)
init = (NoModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Articles"
  , body = 
    [ CDN.stylesheet
    , [ [ [ h1 [] [text "Articles!"] 
          ] |> Grid.col [] 
        ] |> Grid.row []
      , [ [ h2 [] [text "🇺🇸 A taxonomy and platform for anomaly detection"]
          , p [] [ strong [] [text "Authors: "]
                 , text "Gheorghe Sebestyen, Anca Hangan, Czakó Zoltán, Kovács György"
                 ]
          , p [] [ strong [] [text "Read: "]
                 , a [href "https://www.researchgate.net/publication/326707960_A_taxonomy_and_platform_for_anomaly_detection#fullTextFileContent"] [text "🔗 Research Gate"]
                 , text ", "
                 , a [href "/res/anomaly_detection_taxonomy.pdf"] [text "🗎 Download"]
                 ]
          , p [] [ strong [] [text "Abstract: "]
                 , text "There are hundreds of anomaly detection methods developed for different purposes and using a wide range of theoretical backgrounds, from system theory and signal processing towards artificial intelligence techniques. Therefore, it is very difficult for a specialist in a given domain (e.g. finance, industrial engineering, networking, environment monitoring, etc.) to select an anomaly detection method that fits best for a given application. The goal of this paper is to propose a taxonomy for anomaly detection methods and also to present a platform that allows a developer to find and tune a given anomaly detection method that is optimal for an application. The platform provides the basic functionalities needed to acquire, process and visualize multidimensional data collected from different sources, as well as the means to test and compare different anomaly detection techniques."]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "🇺🇸 Platform for Anomaly Detection in Time-Series"]
          , p [] [ strong [] [text "Authors: "]
                 , text "Gheorghe Sebestyen, Anca Hangan, Kovács György, Czakó Zoltán"
                 ]
          , p [] [strong [] [text "Read: "]
                  , a [href "https://www.researchgate.net/publication/334045070_Platform_for_Anomaly_Detection_in_Time-Series"] [text "🔗 Research Gate"]
                  , text ", "
                 , a [href "/res/anomaly_detection_platform.pdf"] [text "🗎 PDF"]
                 
                 ]
          , p [] [ strong [] [text "Abstract: "]
                 , text "This paper presents a platform that integrates a number of functionalities necessary in the process of anomaly detection, from preprocessing towards various anomaly detection techniques and visualization methods. The purpose of this tool is to allow a developer to test, select and fine tune different algorithms that best fit anomaly detection in a given domain. To demonstrate the utility of the platform, we present a series of experiments done with different methods for anomaly detection on time-series and evaluate their results."]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "🇺🇸 Evaluation metrics for anomaly detection algorithms in time-series"]
          , p [] [ strong [] [text "Authors: "]
                 , text "Kovács György, Gheorghe Sebestyen, Anca Hangan"
                 ]
          , p [] [ strong [] [text "Read: "]
                 , a [href "https://www.researchgate.net/publication/338788481_Evaluation_metrics_for_anomaly_detection_algorithms_in_time-series"] [text "🔗 Research Gate"]
                 , text ", "
                 , a [href "/res/anomaly_detection_metrics.pdf"] [text "🗎 PDF"]
                 ]
          , p [] [ strong [] [text "Abstract: "]
                 , text "Time-series are ordered sequences of discrete-time data. Due to their temporal dimension, anomaly detection techniques used in time-series have to take into consideration time correlations and other time related particularities. Generally, in order to evaluate the quality of an anomaly detection technique, the confusion matrix and its derived metrics such as precision and recall are used. These metrics, however, do not take this temporal dimension into consideration. In this paper, we propose three metrics that can be used to evaluate the quality of a classification, while accounting for the temporal dimension found in time-series data."]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "🇷🇴 Monad Transformers"]
          , p [] [ strong [] [text "Author: "]
                 , text "Kovács György"
                 ]
          , p [] [ strong [] [text "Read: "]
                 , a [href "https://www.todaysoftmag.ro/article/2732/monad-transformers"] [text "🔗 Today Software Magazine"]
                 , br [] []
                 , strong [] [text "Watch: "]
                 , a [href "https://youtu.be/3DmxZuSpft4"] [text "📼 Youtube"]
                 ]
          , p [] [ strong [] [text "Abstract: "]
                 , text "Cu toate că pare un concept foarte complicat, Monad Transformer se bazează pe concepte de bază: în mod specific, functorul și monada. Aceste concepte sunt esențiale pentru orice programator funcțional. Având cunoștințele necesare, nu este dificil de înțeles ce este un Monad Transformer și în ce situații s-ar putea folosi."]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "🇷🇴 La fel, dar diferit"]
          , p [] [ strong [] [text "Author: "]
                 , text "Kovács György"
                 ]
          , p [] [ strong [] [text "Read: "]
                 , a [href "https://www.todaysoftmag.ro/article/2939/la-fel-dar-diferit"] [text "🔗 Today Software Magazine"]
                 ]
          , p [] [ strong [] [text "Abstract: "]
                 , text "A programa funcțional este a programa. Consensul programatorilor, mai ales al celor care folosesc programarea funcțională, este că programarea funcțională este viitorul. Ei ne zic că este mai ușor de înțeles codul, pentru că nu trebuie să ținem cont de starea sistemului. Codul e mai succint, pentru că refolosim zeci și poate sute de funcții predefinite care au fost scrise deja și ne permit să ne concentrăm direct pe implementare."]
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }
