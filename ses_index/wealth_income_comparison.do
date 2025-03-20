*************************************************************************
*ACCENTS AS CAPITAL: Wealth and Income comparison for Colombia          *
*Elaborado por: David Becerra											*
*************************************************************************

clear all
cls
*ssc install egenmore
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
           ytitle("Household wealth index (Polychoric PCA)", size(small)) ///
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
           ytitle("Household wealth index (Polychoric PCA)", size(small)) ///
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

