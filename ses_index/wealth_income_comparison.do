*************************************************************************
*ACCENTS AS CAPITAL: Wealth and Income comparison for Colombia          *
*Elaborado por: David Becerra											*
*************************************************************************

clear all
cls

*Paquetes necesarios
*ssc install binscatter
*net install polychoric

* Macros

global Data "C:\Users\dafrb\OneDrive - Universidad de los andes\TREES\Accents_as_Capital\data\ses_index"

global Output "C:\Users\dafrb\OneDrive - Universidad de los andes\TREES\Accents_as_Capital\output\ses_index"

**# 1. DATA CLEAN

* ECV

use "$Data\ECV\Datos de la vivienda.dta", clear

merge 1:1 DIRECTORIO SECUENCIA_ENCUESTA SECUENCIA_P using "$Data\ECV\Condiciones de vida del hogar y tenencia de bienes.dta", gen(_merge)

merge 1:1 DIRECTORIO SECUENCIA_ENCUESTA SECUENCIA_P using "$Data\ECV\Servicios del hogar.dta", gen(_merge2)

drop if _merge==2

rename *, lower

* Recodificar variables de interés a binarias
* Bienes del hogar
gen computer = (p1077s21 == 1 | p1077s22 == 1)
gen washing_machine = (p1077s1 == 1)
gen car = (p1077s15 == 1)
gen water_heater = (p1077s6 == 1)
gen stove = (p1077s3 == 1)
gen motorcycle = (p1077s16 == 1)
gen tv = (p1077s7 == 1 | p1077s8 == 1)
gen vacation_house = (p1077s18 == 1)

* Servicios básicos
gen cable_tv = (p3201 == 1)
gen internet = (p1075 == 1)
gen water_service = (p8520s5 == 1)
gen electricity = (p8520s1 == 1)

* Código del municipio
egen cod_mpio = concat(p1_departamento p1_municipio)

* Re-escalar percapita
gen percapita_millones = percapita / 1000000

keep directorio secuencia_encuesta secuencia_p orden cod_mpio p1_departamento fex_c i_hogar i_ugasto percapita percapita_millones cant_personas_hogar washing_machine car water_heater stove motorcycle tv computer cable_tv electricity internet water_service vacation_house

save "$Data\ECV.dta", replace

**# 2. PROCESSING

* INDICE DE RIQUEZA
use "$Data\ECV.dta", clear

* Análisis de correspondencias múltiples (MCA)
mca washing_machine car water_heater stove motorcycle tv computer cable_tv electricity internet water_service vacation_house, method(burt)

* Sacar scores
predict mc1

* Normalizar el índice
sum mc1, detail
gen wealth_index = (mc1 - r(min)) / (r(max) - r(min)) * 100

* Análisis de Componentes Principales (PCA)
pca washing_machine car water_heater stove motorcycle tv computer cable_tv electricity internet water_service vacation_house

* Predecir el primer componente principal (que suele capturar la mayor variación)
predict pc1

* Normalizar el índice PCA para que vaya de 0 a 100
sum pc1
gen wealth_index_pca = (pc1 - r(min)) / (r(max) - r(min)) * 100

* Análisis de Componentes Principales Policóricos (Polychoric pca)
polychoricpca washing_machine car water_heater stove motorcycle tv computer electricity internet water_service vacation_house, score(polpca) nscore(1)

* Sacar scores
predict polpc1

* Normalizar el índice
sum polpca, detail
gen wealth_index_polpca = (polpca - r(min)) / (r(max) - r(min)) * 100
sum wealth_index_polpca, detail

* Comparar correlaciones entre ambos índices e ingreso
corr wealth_index_polpca wealth_index_pca pc1 percapita

* Descriptivas del nuevo índice
sum wealth_index_pca, detail

*Guardar
save "$Data/ECV_wealth_index.dta", replace

**# 3. OUTPUT

* Análisis índice de Polychoric PCA
use "$Data/ECV_wealth_index.dta", clear

* Eliminar valores extremos globalmente
keep if percapita < 50000000

* Lista de ciudades principales para comparar
local ciudades "11001 05001 76001 08001 13001"
local nombres "Bogotá Medellín Cali Barranquilla Cartagena"

* Gráficos con regresión ajustada (Polychoric PCA)
forvalues i = 1/5 {
    local ciudad: word `i' of `ciudades'
    local nombre: word `i' of `nombres'
    
    preserve
    keep if cod_mpio == "`ciudad'"
    
    * Calcular correlación usando factor de expansión
    gen fex_round = round(fex_c)
    expand fex_round
    corr wealth_index_polpca percapita
    local corr_`i' = r(rho)
    local corr_`i'_text = string(`corr_`i'', "%9.3f")
    
    * Cargar otra vez datos
    restore
    preserve
    keep if cod_mpio == "`ciudad'"
    
    * Máximo de ingreso por ciudad
    quietly summarize percapita
    local max_income = r(max)
    local x_max = min(`max_income' * 1.1, 50000000)
    
    * Breaks para el gráfico
    local x_max_m = `x_max' / 1000000
    local x_break_m = round(`x_max_m'/4, 0.5)
    
    * Crear gráfico
    twoway (scatter wealth_index_polpca percapita_millones, mcolor(navy) msize(small) msymbol(circle)) ///
           (lfit wealth_index_polpca percapita_millones, lcolor(red)), ///
           title("`nombre' (Polychoric PCA)", size(medium)) ///
           subtitle("Correlation: `corr_`i'_text'", size(small)) ///
           xtitle("Monthly per-capita income (millions COP)", size(small)) ///
           ytitle("Household wealth index (Polychoric PCA)", size(vsmall)) ///
           ylabel(0(20)100, format(%9.0fc) labsize(small)) ///
           yscale(range(0 100)) ///
           xlabel(0(`x_break_m')`x_max_m', format(%3.1f) labsize(small)) ///
           xscale(range(0 `x_max_m')) ///
           graphregion(color(white) margin(medium)) bgcolor(white) ///
           plotregion(margin(medium)) ///
           legend(off)
    
    * Guardar y exportar
    graph save "$Output/g_polpca_with_reg_`i'.gph", replace
	graph export "$Output/g_polpca_with_reg_`i'.png", replace
    restore
}

* Combinar los gráficos
graph combine "$Output/g_polpca_with_reg_1.gph" "$Output/g_polpca_with_reg_2.gph" "$Output/g_polpca_with_reg_3.gph" ///
    "$Output/g_polpca_with_reg_4.gph" "$Output/g_polpca_with_reg_5.gph", ///
    cols(2) xsize(12) ysize(15) ///
    title("Polychoric PCA wealth index vs per-capita income with regression line", size(small)) ///
    graphregion(color(white) margin(large)) ///
    note("Note: All correlations calculated using sampling weights", size(small))

* Guardar
graph export "$Output/cities_comparison_polpca_with_reg.png", replace width(2400) height(3000)

* Gráficos sin regresión (Polychoric PCA)
forvalues i = 1/5 {
    local ciudad: word `i' of `ciudades'
    local nombre: word `i' of `nombres'
    
    preserve
    keep if cod_mpio == "`ciudad'"
    
    * Retomar local de correlación
    local corr_text = `corr_`i'_text'
    
    * Máximo ingreso por ciudad
    quietly summarize percapita
    local max_income = r(max)
    local x_max = min(`max_income' * 1.1, 50000000) 
    
    * Breaks para el gráfico
    local x_max_m = `x_max' / 1000000
    local x_break_m = round(`x_max_m'/4, 0.5)
    
    * Crear gráfico
    twoway (scatter wealth_index_polpca percapita_millones, mcolor(navy) msize(small) msymbol(circle)), ///
           title("`nombre' (Polychoric PCA)", size(medium)) ///
           subtitle("Correlation: `corr_text'", size(small)) ///
           xtitle("Monthly per-capita income (millions COP)", size(small)) ///
           ytitle("Household wealth index (Polychoric PCA)", size(vsmall)) ///
           ylabel(0(20)100, format(%9.0fc) labsize(small)) ///
           yscale(range(0 100)) ///
           xlabel(0(`x_break_m')`x_max_m', format(%3.1f) labsize(small)) ///
           xscale(range(0 `x_max_m')) ///
           graphregion(color(white) margin(medium)) bgcolor(white) ///
           plotregion(margin(medium)) ///
           legend(off)
    
    * Guardar
    graph save "$Output/g_polpca_no_reg_`i'.gph", replace
	graph export "$Output/g_polpca_no_reg_`i'.png", replace
    restore
}

* Combinar los gráficos
graph combine "$Output/g_polpca_no_reg_1.gph" "$Output/g_polpca_no_reg_2.gph" "$Output/g_polpca_no_reg_3.gph" ///
    "$Output/g_polpca_no_reg_4.gph" "$Output/g_polpca_no_reg_5.gph", ///
    cols(2) xsize(12) ysize(15) ///
    title("Polychoric PCA wealth index vs per-capita income (scatter only)", size(small)) ///
    graphregion(color(white) margin(large)) ///
    note("Note: All correlations calculated using sampling weights", size(small))

* Guardar
graph export "$Output/cities_comparison_polpca_no_reg.png", replace width(2400) height(3000)

**# 4. NUEVAS VARIACIONES

/* Incluir tres variaciones

1. Comparar Bogotá vs 12 otras ciudades principales

2. Eliminar outliers (influencial obs y extreme values)

3. Scatter plot y Binscatter

*/

* 4.1. Filtrar Bogotá vs 12 ciudades principales

* Medellín A.M (Medellín, Caldas, La Estrella, Sabaneta, Itagüí, Envigado, Bello, Copacabana, Girardota y Barbosa), Cali A.M (Cali, Yumbo), Barranquilla A.M (Barranquilla, Soledad), Bucaramanga A.M (Bucaramanga, Florida, Girón, Piedecuesta), Manizales A.M (Manizales, Villamaría), Pasto, Pereira A.M (Pereira, Dosquebradas, La Virginia), Ibagué, Cúcuta A.M (Cúcuta, Los Patios, Villa del Rosario, El Zulia), Villavicencio, Montería, Cartagena

* Cargar datos
use "$Data/ECV_wealth_index.dta", clear

* Filtrar por lista de ciudades principales
keep if cod_mpio == "05001" | cod_mpio == "05129" | cod_mpio == "05380" | /// 
cod_mpio == "05631" | cod_mpio == "05360" | cod_mpio == "05266" | ///
cod_mpio == "05088" | cod_mpio == "05212" | cod_mpio == "05308" | ///
cod_mpio == "05079" | cod_mpio == "76001" | cod_mpio == "76892" | ///
cod_mpio == "08001" | cod_mpio == "08758" | cod_mpio == "68001" | ///
cod_mpio == "76275" | cod_mpio == "68307" | cod_mpio == "68547" | ///
cod_mpio == "17001" | cod_mpio == "17873" | cod_mpio == "52001" | ///
cod_mpio == "66001" | cod_mpio == "66170" | cod_mpio == "66400" | ///
cod_mpio == "73001" | cod_mpio == "54001" | cod_mpio == "54405" | ///
cod_mpio == "54874" | cod_mpio == "54261" | cod_mpio == "50001" | ///
cod_mpio == "23001" | cod_mpio == "13001" | cod_mpio == "11001"

gen bogota = (cod_mpio == "11001")

preserve
keep if bogota == 1
save "$Data/wealth_ind_bog.dta", replace
restore

preserve
keep if bogota == 0
save "$Data/wealth_ind_other.dta", replace
restore

* 4.2. Outlier handling

** Crear indicadores para cada medida de outliers

foreach file in wealth_ind_bog.dta wealth_ind_other.dta {

	use "C:/Users/dafrb/OneDrive - Universidad de los andes/TREES/Accents_as_Capital/data/ses_index/`file'", clear

	* Filtrando por extreme values: Voy a probar 2 approaches (sd y iqr)

	** Filtro con desviación  estándar (Regla de 3 desviaciones estándar)
	sum percapita, detail

	** Bounds para std dev
	gen lower_bound = r(mean) - 3*r(sd)
	gen upper_bound = r(mean) + 3*r(sd)

	gen ext_sd = (percapita < lower_bound | percapita > upper_bound)
	sum ext_sd

	* Filtro con rango intercuartílico (1.5 el iqr)
	sum percapita, detail

	** Cálculo del iqr
	scalar q1 = r(p25)
	scalar q3 = r(p75)
	scalar iqr = q3 - q1

	** Bounds para iqr
	gen lower_bound_iqr = q1 - 1.5*iqr
	gen upper_bound_iqr = q3 + 1.5*iqr

	** Generar indicador
	gen ext_iqr = (percapita < lower_bound_iqr | percapita > upper_bound_iqr)
	
	* Filtrando por influential observations: DFits y Distancia de Cook

	* Filtro con DFits
	** Regresión
	reg wealth_index_polpca percapita

	** Calcular DFITS
	predict dfits, dfits

	** Umbral sugerido para DFITS: 2*sqrt(k/n) donde k=número de predictores
	scalar crit_dfit = 2*sqrt((e(df_m)+2)/(e(N)-e(df_m)-2))
	gen infobs_dfit = (abs(dfits) > crit_dfit)
	sum infobs_dfit

	** Regresión
	reg wealth_index_polpca percapita

	** Calcular distancia de Cook
	predict cooksd, cooksd

	** Indicador
	scalar crit_cook = 4/e(N)
	gen infobs_cook = (cooksd > crit_cook)

	list wealth_index_polpca percapita if ext_sd==1 & ext_iqr==1 & infobs_dfit==1 & infobs_cook==1

	tab ext_sd ext_iqr
	tab infobs_dfit infobs_cook

	save "C:/Users/dafrb/OneDrive - Universidad de los andes/TREES/Accents_as_Capital/data/ses_index/`file'", replace

}

** Graficar los indicadores para observar los puntos

* Macro archivos
local files wealth_ind_bog.dta wealth_ind_other.dta

* Macro outliers
local outlier_vars ext_sd ext_iqr infobs_dfit infobs_cook

* Macro para títulos de los gráficos
local titles "Filtro por 3 desviaciones estándar" "Filtro por 1.5 IQR" "Filtro por DFITS" "Filtro por distancia de Cook"

* Bucle para cada archivo
foreach file of local files {
    use "C:/Users/dafrb/OneDrive - Universidad de los andes/TREES/Accents_as_Capital/data/ses_index/`file'", clear
    
    * Extraer parte del nombre del archivo para incluir en los nombres de los gráficos
    local file_suffix = subinstr("`file'", ".dta", "", .)
    
    * Bucle para cada variable de outlier
    forvalues i = 1/4 {
        local var : word `i' of `outlier_vars'
        local title : word `i' of `titles'
        
        * Generar el scatterplot con dos capas
        twoway (scatter wealth_index_polpca percapita if `var'==0, mcolor(blue) msize(small)) ///
               (scatter wealth_index_polpca percapita if `var'==1, mcolor(red) msize(medium) msymbol(Oh)), ///
               title("`title' - `file_suffix'") ///
               subtitle("Puntos rojos = outliers identificados") ///
               xtitle("Ingreso per cápita") ytitle("Índice de riqueza") ///
               legend(order(1 "Observaciones regulares" 2 "Outliers"))
        
        * Guardar el gráfico
        graph export "$Output/graphs/scatter_`var'_`file_suffix'.png", replace width(1000)
    
    * Generar un gráfico adicional que muestre todos los métodos en un solo gráfico
    gen any_outlier = (ext_sd==1 | ext_iqr==1 | infobs_dfit==1 | infobs_cook==1)
    gen common_outlier = (ext_sd==1 & ext_iqr==1 & infobs_dfit==1 & infobs_cook==1)
    
    twoway (scatter wealth_index_polpca percapita if any_outlier==0, mcolor(blue) msize(small)) ///
           (scatter wealth_index_polpca percapita if any_outlier==1 & common_outlier==0, mcolor(orange) msize(small) msymbol(O)) ///
           (scatter wealth_index_polpca percapita if common_outlier==1, mcolor(red) msize(medium) msymbol(Oh)), ///
           title("Comparación de métodos - `file_suffix'") ///
           xtitle("Ingreso per cápita") ytitle("Índice de riqueza") ///
           legend(order(1 "No outliers" 2 "Algunos métodos" 3 "Todos los métodos"))
    
    graph export "$Output/graphs/scatter_comparison_`file_suffix'.png", replace width(1000)
	
	}
}

** Me voy a quedar con sd-filter y con cook's distance.

** Bucle para crear versiones de los archivos con ambos filtros
foreach file in wealth_ind_bog wealth_ind_other {
    use "C:/Users/dafrb/OneDrive - Universidad de los andes/TREES/Accents_as_Capital/data/ses_index/`file'.dta", clear
    
    foreach var in ext_sd infobs_cook {
        * Filtrando por extreme values: Voy a probar 2 approaches (sd y iqr)
        preserve
        keep if `var' == 0
        save "C:/Users/dafrb/OneDrive - Universidad de los andes/TREES/Accents_as_Capital/data/ses_index/`file'_`var'.dta", replace
        restore
    }
}

**# 4.3. Scatter plots y bin scatter

* Definir macros para los tipos de filtros
local filtros ext_sd infobs_cook
local nombres_filtros "Filtro SD (3σ)" "Filtro Cook's D"

* Definir colores y símbolos
local color_bog "navy"
local color_other "maroon"
local symbol_bog "circle"
local symbol_other "diamond"

* Inicializar nombres de los gráficos guardados
local scatter_graphs_sd ""
local scatter_graphs_cook ""
local bin_graphs_sd ""
local bin_graphs_cook ""

* Bucle para cada tipo de filtro
foreach filtro of local filtros {
    local i = 0
    foreach f of local filtros {
        local i = `i' + 1
        if "`f'" == "`filtro'" {
            local nombre_filtro : word `i' of `nombres_filtros'
        }
    }
    
    * Bogotá
    * Cargar datos
    use "$Data/wealth_ind_bog_`filtro'.dta", clear
    
    * Calcular correlación usando factor de expansión
    gen fex_round = round(fex_c)
    expand fex_round
    corr wealth_index_polpca percapita
    local corr_bog = r(rho)
    local corr_bog_text = string(`corr_bog', "%9.3f")
    
    * Restaurar datos originales de Bogotá
    use "$Data/wealth_ind_bog_`filtro'.dta", clear
    
    * Datos para ejes scatterplot
    summarize percapita_millones, detail
    local max_income_bog_scatter = min(r(max) * 1.1, 50)
    local x_break_bog_scatter = round(`max_income_bog_scatter'/5, 0.5)
    
    * Datos para ejes binscatter
    tempfile orig_data
    save `orig_data', replace
    local max_income_bog_bin = min(r(p99) * 1.2, 11)
    local x_break_bog_bin = round(`max_income_bog_bin'/5, 0.5)
    
    * Restaurar los datos originales
    use "$Data/wealth_ind_bog_`filtro'.dta", clear
    
    ****** 1. SCATTER PLOT BOGOTÁ ******
    twoway (scatter wealth_index_polpca percapita_millones, ///
        mcolor(`color_bog') msize(small) msymbol(`symbol_bog')), ///
       title("Bogotá", size(medium)) ///
       subtitle("Correlación =  `corr_bog_text'", size(small)) ///
       xtitle("Ingreso mensual per cápita (millones COP)", size(small)) ///
       ytitle("Índice de riqueza (PCA Policórico)", size(small)) ///
       ylabel(0(20)100, format(%9.0fc) labsize(small) nogrid) ///
       yscale(range(0 100)) ///
       xlabel(0(`x_break_bog_scatter')`max_income_bog_scatter', format(%3.1f) labsize(small) nogrid) ///
       xscale(range(0 `max_income_bog_scatter')) ///
       graphregion(color(white) margin(medium)) bgcolor(white) ///
       plotregion(margin(medium)) ///
       legend(off)
    
    * Guardar gráfico
    graph save "$Output/temp_scatter_bog_`filtro'.gph", replace
    
    if "`filtro'" == "ext_sd" {
        local scatter_graphs_sd "`scatter_graphs_sd' "$Output/temp_scatter_bog_`filtro'.gph""
    }
    else {
        local scatter_graphs_cook "`scatter_graphs_cook' "$Output/temp_scatter_bog_`filtro'.gph""
    }
    
    ****** 2. BINSCATTER BOGOTÁ ******

        binscatter wealth_index_polpca percapita_millones, nquantiles(20) ///
        msymbol(`symbol_bog') ///
        mcolor(`color_bog') linetype(lfit) ///
        title("Bogotá", size(medium)) ///
        subtitle("Correlación =  `corr_bog_text'", size(small)) ///
        xtitle("Ingreso mensual per cápita (millones COP)", size(small)) ///
        ytitle("Índice de riqueza (PCA Policórico)", size(small)) ///
        ylabel(50(10)100, format(%9.0fc) labsize(small) nogrid) ///
        yscale(range(50 100)) ///
        xlabel(0(`x_break_bog_bin')`max_income_bog_bin', format(%3.1f) labsize(small) nogrid) ///
        xscale(range(0 `max_income_bog_bin')) ///
        graphregion(color(white) margin(medium)) bgcolor(white) ///
        plotregion(margin(medium)) ///
        legend(off)
        
        * Guardar gráfico temporal
        graph save "$Output/temp_bin_bog_`filtro'.gph", replace
        
        if "`filtro'" == "ext_sd" {
            local bin_graphs_sd "`bin_graphs_sd' "$Output/temp_bin_bog_`filtro'.gph""
        }
        else {
            local bin_graphs_cook "`bin_graphs_cook' "$Output/temp_bin_bog_`filtro'.gph""
        }
    
    * Otras 12 ciudades
    * Cargar datos
    use "$Data/wealth_ind_other_`filtro'.dta", clear
    
    * Calcular correlación
    gen fex_round = round(fex_c)
    expand fex_round
    corr wealth_index_polpca percapita
    local corr_other = r(rho)
    local corr_other_text = string(`corr_other', "%9.3f")
    
    * Volver a cargar datos
    use "$Data/wealth_ind_other_`filtro'.dta", clear
    
    * Ejes scatter plots
    summarize percapita_millones, detail
    local max_income_other_scatter = min(r(max) * 1.1, 50)
    local x_break_other_scatter = round(`max_income_other_scatter'/5, 0.5)
    
    * Ejes binscatter
    local max_income_other_bin = min(r(p99) * 1.2, 7)
    local x_break_other_bin = round(`max_income_other_bin'/5, 0.5)
    
    * Restaurar los datos originales
    use "$Data/wealth_ind_other_`filtro'.dta", clear
    
    ****** 3. SCATTER PLOT OTRAS CIUDADES ******
    twoway (scatter wealth_index_polpca percapita_millones, ///
        mcolor(`color_other') msize(small) msymbol(`symbol_other')), ///
       title("Otras 12 ciudades principales", size(medium)) ///
       subtitle("Correlación = `corr_other_text'", size(small)) ///
       xtitle("Ingreso mensual per cápita (millones COP)", size(small)) ///
       ytitle("Índice de riqueza (PCA Policórico)", size(small)) ///
       ylabel(0(20)100, format(%9.0fc) labsize(small) nogrid) ///
       yscale(range(0 100)) ///
       xlabel(0(`x_break_other_scatter')`max_income_other_scatter', format(%3.1f) labsize(small) nogrid) ///
       xscale(range(0 `max_income_other_scatter')) ///
       graphregion(color(white) margin(medium)) bgcolor(white) ///
       plotregion(margin(medium)) ///
       legend(off)
    
    * Guardar gráfico temporal
    graph save "$Output/temp_scatter_other_`filtro'.gph", replace
    
    if "`filtro'" == "ext_sd" {
        local scatter_graphs_sd "`scatter_graphs_sd' "$Output/temp_scatter_other_`filtro'.gph""
    }
    else {
        local scatter_graphs_cook "`scatter_graphs_cook' "$Output/temp_scatter_other_`filtro'.gph""
    }
    
    ****** 4. BINSCATTER OTRAS CIUDADES ******
    
        binscatter wealth_index_polpca percapita_millones, nquantiles(20) ///
        msymbol(`symbol_other') ///
        mcolor(`color_other') linetype(lfit) ///
        title("Otras 12 ciudades principales", size(medium)) ///
        subtitle("Correlación = `corr_other_text'", size(small)) ///
        xtitle("Ingreso mensual per cápita (millones COP)", size(small)) ///
        ytitle("Índice de riqueza (PCA Policórico)", size(small)) ///
        ylabel(50(10)100, format(%9.0fc) labsize(small) nogrid) ///
        yscale(range(50 100)) ///
        xlabel(0(`x_break_other_bin')`max_income_other_bin', format(%3.1f) labsize(small) nogrid) ///
        xscale(range(0 `max_income_other_bin')) ///
        graphregion(color(white) margin(medium)) bgcolor(white) ///
        plotregion(margin(medium)) ///
        legend(off)
        
        * Guardar gráfico temporal
        graph save "$Output/temp_bin_other_`filtro'.gph", replace
        
        if "`filtro'" == "ext_sd" {
            local bin_graphs_sd "`bin_graphs_sd' "$Output/temp_bin_other_`filtro'.gph""
        }
        else {
            local bin_graphs_cook "`bin_graphs_cook' "$Output/temp_bin_other_`filtro'.gph""
        }
}

* Combinar los gráficos en los cuatro resultados finales

* 1. Scatter plots con filtro SD
graph combine `scatter_graphs_sd', ///
    cols(2) xsize(12) ysize(9) ///
    title("Scatter plot: Índice de riqueza vs. ingreso per cápita (Filtro SD)", size(medium)) ///
    graphregion(color(white) margin(large)) ///
    note("Nota: Correlaciones calculadas usando factores de expansión", size(small))
graph export "$Output/combined_scatter_sd.png", replace width(2400) height(1800)

* 2. Scatter plots con filtro Cook
graph combine `scatter_graphs_cook', ///
    cols(2) xsize(12) ysize(9) ///
    title("Scatter plot: Índice de riqueza vs. ingreso per cápita (Filtro Cook's D)", size(medium)) ///
    graphregion(color(white) margin(large)) ///
    note("Nota: Correlaciones calculadas usando factores de expansión", size(small))
graph export "$Output/combined_scatter_cook.png", replace width(2400) height(1800)

* 3. Binscatter plots con filtro SD
graph combine `bin_graphs_sd', ///
    cols(2) xsize(12) ysize(9) ///
    title("Binscatter: Índice de riqueza vs. ingreso per cápita (Filtro SD)", size(medium)) ///
    graphregion(color(white) margin(large)) ///
    note("Nota: Binscatter con 20 bins. Correlaciones calculadas usando factores de expansión", size(small))
graph export "$Output/combined_binscatter_sd.png", replace width(2400) height(1800)

* 4. Binscatter plots con filtro Cook
graph combine `bin_graphs_cook', ///
    cols(2) xsize(12) ysize(9) ///
    title("Binscatter: Índice de riqueza vs. ingreso per cápita (Filtro Cook's D)", size(medium)) ///
    graphregion(color(white) margin(large)) ///
    note("Nota: Binscatter con 20 bins. Correlaciones calculadas usando factores de expansión", size(small))
graph export "$Output/combined_binscatter_cook.png", replace width(2400) height(1800)

}
