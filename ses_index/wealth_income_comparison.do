*************************************************************************
*ACCENTS AS CAPITAL: Wealth and Income comparison for Colombia          *
*Elaborado por: David Becerra											*
*************************************************************************

clear all
cls
ssc install egenmore

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
gen refrigerator = (p1077s2 == 1)
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

keep directorio secuencia_encuesta secuencia_p orden cod_mpio p1_departamento fex_c i_hogar percapita cant_personas_hogar washing_machine refrigerator car water_heater stove motorcycle tv computer cable_tv electricity internet water_service vacation_house

save "$Data\ECV.dta", replace

* GEIH

local meses "enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre"
foreach x of local meses {
    use "$Data/GEIH/`x'.dta", clear
    rename *, lower
    save "$Data/GEIH/`x'.dta", replace
}

use "$Data/GEIH/enero.dta", clear

local meses "febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre"
foreach x of local meses {
    append using "$Data/GEIH/`x'.dta"
}

save "$Data/GEIH.dta", replace

**# 2. PROCESSING

* INDICE DE RIQUEZA
use "$Data\ECV.dta", clear

* Análisis de correspondencias múltiples (MCA)
mca washing_machine refrigerator car water_heater stove motorcycle tv computer cable_tv electricity internet water_service vacation_house, method(burt)

* Sacar scores
predict wealthindex

* Normalizar el índice
sum wealthindex
gen wealth_index = (wealthindex - r(min)) / (r(max) - r(min)) * 100

* Algunas descriptivas
sum wealth_index, detail

*Guardar
save "$Data/ECV_wealth_index.dta", replace

* Agregar por departamento (Análisis solo ECV y ECV vs GEIH)
preserve
collapse (mean) wealth_index percapita [iw=fex_c], by(p1_departamento)
rename p1_departamento cod_depto
save "$Data/collapse_wealth_index.dta", replace
restore

* INGRESO LABORAL
use "$Data/GEIH.dta", clear

* Agregar por departamento
collapse (mean) inglabo [iw=fex_c], by(dpto)
rename dpto cod_depto

* Guardar
save "$Data/income_dpto.dta", replace

**# 3. OUTPUT

* Plots de Ingresos vs Riqueza

* Approach 1. Solo ECV a nivel municipio
use "$Data/ECV_wealth_index.dta", clear

* Agregar por municipio
collapse (mean) wealth_index percapita [iw=fex_c], by(cod_mpio)

* Calcular correlación general
corr wealth_index percapita
local corr = r(rho)
local corr_text = string(`corr', "%9.3f")

* Calcular correlación sin Bogotá
corr wealth_index percapita if cod_mpio != "11"
local corr_sin_bogota = r(rho)
local corr_sin_bogota_text = string(`corr_sin_bogota', "%9.3f")

* Scatter Riqueza vs Ingreso
twoway (scatter wealth_index percapita if cod_mpio != "11001", mcolor(gray) msize(medium)) ///
       (scatter wealth_index percapita if cod_mpio == "11001", mcolor(orange) msize(large) msymbol(diamond)) ///
       (lfit wealth_index percapita, lcolor(navy)), ///
       title("Figure 1: Wealth index vs per-capita income by municipality", size(large)) ///
       xtitle("Average monthly per-capita income (in COP)", size(medium)) ///
       ytitle("Average wealth index", size(medium)) ///
       text(90 1800000 "Correlation Colombia: `corr_text'", place(e) size(medium)) ///
       text(85 1800000 "Correlation without Bogotá: `corr_sin_bogota_text'", place(e) size(medium)) ///
       xlabel(0 1000000 2000000 3000000 4000000, format(%12.0fc) labsize(medium)) ///
       ylabel(0(20)100, format(%9.0fc) labsize(medium)) ///
       xscale(range(800000 2800000)) ///
       graphregion(color(white) margin(medium)) bgcolor(white) ///
       plotregion(margin(medium)) ///
       legend(off) ///
       text(32 2300000 "Bogotá", color(red) size(medium))

* Guardar el gráfico
graph export "$Output/corr_wealth_income_mun.png", replace width(1200) height(900)

* Approach 2. Solo ECV a nivel departamento

use "$Data/collapse_wealth_index.dta", clear

* Calcular correlación general
corr wealth_index percapita
local corr = r(rho)
local corr_text = string(`corr', "%9.3f")

* Calcular correlación sin Bogotá
corr wealth_index percapita if cod_depto != "11"
local corr_sin_bogota = r(rho)
local corr_sin_bogota_text = string(`corr_sin_bogota', "%9.3f")

* Generar el gráfico de dispersión con línea de regresión y resaltar Bogotá
twoway (scatter wealth_index percapita if cod_depto != "11", mcolor(gray) msize(medium)) ///
       (scatter wealth_index percapita if cod_depto == "11", mcolor(orange) msize(large) msymbol(diamond)) ///
       (lfit wealth_index percapita, lcolor(navy)), ///
       title("Figure 2: Wealth index vs per-capita income by department", size(large)) ///
       xtitle("Average monthly per-capita income (in COP)", size(medium)) ///
       ytitle("Average wealth index", size(medium)) ///
       text(90 1200000 "Correlation Colombia: `corr_text'", place(e) size(medium)) ///
       text(85 1200000 "Correlation without Bogotá: `corr_sin_bogota_text'", place(e) size(medium)) ///
       xlabel(500000 1000000 1500000 2000000, format(%12.0fc) labsize(medium)) ///
       ylabel(0(20)100, format(%9.0fc) labsize(medium)) ///
       xscale(range(800000 2800000)) ///
       graphregion(color(white) margin(medium)) bgcolor(white) ///
       plotregion(margin(medium)) ///
       legend(off) ///
       text(30 2300000 "Bogotá", color(red) size(medium))

* Guardar el gráfico
graph export "$Output/corr_wealth_income_depto.png", replace width(1200) height(900)

* Approach 3. ECV vs GEIH a nivel departamento

use "$Data/wealth_index.dta", clear
merge 1:1 cod_depto using "$Data/income_dpto.dta"

* Calcular correlación general
corr wealth_index inglabo
local corr = r(rho)
local corr_text = string(`corr', "%9.3f")

* Calcular correlación sin Bogotá
corr wealth_index inglabo if cod_depto != "11"
local corr_sin_bogota = r(rho)
local corr_sin_bogota_text = string(`corr_sin_bogota', "%9.3f")

* Generar el gráfico de dispersión con línea de regresión y resaltar Bogotá
twoway (scatter wealth_index inglabo if cod_depto != "11", mcolor(gray) msize(medium)) ///
       (scatter wealth_index inglabo if cod_depto == "11", mcolor(orange) msize(large) msymbol(diamond)) ///
       (lfit wealth_index inglabo, lcolor(navy)), ///
       title("Figure 3: Wealth index vs labor income", size(large)) ///
       xtitle("Average monthly labor income (in COP)", size(medium)) ///
       ytitle("Average wealth index", size(medium)) ///
       text(90 1800000 "Correlation Colombia: `corr_text'", place(e) size(medium)) ///
       text(85 1800000 "Correlation without Bogotá: `corr_sin_bogota_text'", place(e) size(medium)) ///
       xlabel(800000 1200000 1600000 2000000 2400000, format(%12.0fc) labsize(medium)) ///
       ylabel(0(20)100, format(%9.0fc) labsize(medium)) ///
       xscale(range(800000 2800000)) ///
       graphregion(color(white) margin(medium)) bgcolor(white) ///
       plotregion(margin(medium)) ///
       legend(off) ///
       text(20 2650000 "Bogotá", color(red) size(medium))

* Guardar el gráfico
graph export "$Output/corr_wealth_income_geih.png", replace width(1200) height(900)