% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readIFN3IFN4.R
\name{readIFN3IFN4}
\alias{readIFN3IFN4}
\alias{readDatosMap}
\alias{readMatorral}
\alias{readRegenera}
\alias{readPiesMayores}
\alias{readPCParcela}
\title{Reads IFN3 and IFN4 data}
\usage{
readDatosMap(source_path, ifn = 3, prov = NULL, ccaa = NULL, plotType = NULL)

readMatorral(source_path, ifn = 3, prov = NULL, ccaa = NULL, plotType = NULL)

readRegenera(source_path, ifn = 3, prov = NULL, ccaa = NULL, plotType = NULL)

readPiesMayores(
  source_path,
  ifn = 3,
  prov = NULL,
  ccaa = NULL,
  plotType = NULL
)

readPCParcela(source_path, ifn = 3, prov = NULL, ccaa = NULL, plotType = NULL)
}
\arguments{
\item{source_path}{Path to the location of BBDD-Campo}

\item{ifn}{Either 3 or 4 (for IFN3 or IFN4, respectively)}

\item{prov}{Character vector with province codes (e.g. c("03", "14", "25"))}

\item{ccaa}{Character vector with names of autonomous communities (e.g. "Catal)}

\item{plotType}{Subset of plot types to include:
\itemize{
\item{N - Se levantan por primera vez en el IFN-3 y son utilizadas para el calculo de existencias actuales.}
\item{A - Parcelas levantadas en el IFN-2 que volveran a ser apeadas en el IFN-3. Existen cinco casos:
  \itemize{
    \item{
       A1 . Parcelas en las que se localice el rejon y esta bien implantado. Se utilizan para la comparacion de inventarios y para el calculo de existencias actuales.
    }
    \item{
       A3 . Parcela apeada en IFN2, pinchazo IFN2 y coordenada UTM situadas en diferentes teselas. Se cumplimentan dos estadillos:
       \itemize{
         \item{
           A3C . Parcela para la Comparacion de Inventarios. Se levantan en el punto donde se encuentra el rejon del IFN-2.
         }
         \item{
           A3E. Parcelas para el calculo de Existencias Actuales. Se levantan en el emplazamiento del IFN-3 ( Coordenada UTM)
         }
       }
    }
    \item{
      A4 . Parcelas en las que no se localizo el rejon. Se utilizan para el calculo de existencias actuales.
    }
    \item{
      A4C . Parcelas en las que no se localice el rej?n por cambio totales en la cubierta forestal de la tesela que contiene la parcela (p?rdida total de la masa
muestreada en el IFN2 o incorporaci?n de una nueva masa forestal inexistente en el IFN2). Se utilizar?n para la comparaci?n de inventarios y para el c?lculo de existencias actuales.
    }
    \item{
      A6C . Parcelas en las que no se localice el rej?n por cambios totales en la
cubierta forestal de la tesela que contiene la parcela (p?rdida total de la masa
muestreada en el IFN2 e incorporaci?n de una nueva masa forestal). Se utilizar?n para la comparaci?n de inventarios y para el c?lculo de existencias actuales.
    }
  }
}
}}
}
\value{
a data frame with IFN data, depending on the function
}
\description{
Reads IFN3 and IFN4 data
}
