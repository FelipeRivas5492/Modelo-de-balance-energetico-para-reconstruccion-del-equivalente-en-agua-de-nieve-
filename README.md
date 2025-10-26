<strong>MODELO DE BALANCE ENERGÉTICO PARA RECONSTRUCCIÓN DEL EQUIVALENTE EN AGUA DE NIEVE. BASADO EN:</strong>


<p align="justify">
<strong>Cline, D. W., Bales, R. C., & Dozier, J. (1998). Estimating the spatial distribution of snow in mountain basins using remote sensing and energy balance modeling. Water Resources Research, 34(5), 1275–1285. https://doi.org/10.1029/97WR03755</strong>
</p>


<p align="justify"> 
<strong>Fassnacht, S. R., Sexstone, G. A., Kashipazha, A. H., López-Moreno, J. I., Jasinski, M. F., Kampf, S. K., & Von Thaden, B. C. (2016). Deriving snow-cover depletion curves for different spatial scales from remote sensing and snow telemetry data. Hydrological Processes, 30(11), 1708–1717. https://doi.org/10.1002/hyp.10730</strong> 
</p> 

<p align="justify"> 
1. El modelo se basa en el balance de los flujos de energía del manto. La idea central del método es establecer mediante observación satelital o empírica cuándo la cobertura de nieve es cero. Con ello se obtiene el último día de derretimiento del manto, mientras que el primer día de derretimiento corresponde al máximo observado del equivalente en agua nieve (SWE). Para este ejemplo no se usó el último día de precipitación sólida, ya que fue pocos días antes del último día de derretimiento y no representa el inicio del derretimiento para esta zona y tiempo analizado en particular. El primer dia de derretimiento corresponde al dia del máximo SWE observado.
</p>

<p align="justify">
 2. Con lo anterior, se obtienen los días en los cuales se generó derretimiento. Luego, con información meteorológica de estaciones próximas es posible resolver el balance de energía y considerar que la energía neta luego del balance se transforma en derretimiento. Para ajustar el modelo se consideró la estación Quebrada Larga Cota 3500, cuya altitud es de 3500 m s. n. m. Notar que ajustar el modelo depende principalmente de las rugosidades de la temperatura, nieve y la consideración de flujos turbulentos o no.
</p> 


<p align="justify">
3. No se consideraron los flujos energéticos entre el manto y el suelo y entre el manto y la precipitación caída en derretimiento. Se consideraron dos modelos para ilustrar la importancia de la corrección atmosférica en altitud en el balance con respecto al calor latente. En el código R del balance, es posible examinar los flujos en detalle. En la <strong>Figura 1</strong> se muestran los resultados del modelo para la estación.
</p>


<div align="center">
  <img src="https://raw.githubusercontent.com/FelipeRivas5492/Modelo-de-balance-energetico-para-reconstruccion-del-equivalente-en-agua-de-nieve-/main/balance.png" alt="Figura 1 - fig1">
<div align="center">
 
<p align="justify">
<strong>Figura 1</strong>: gráfico derecha: SWE retrospectivo para modelos con y sin corrección atmosférica y SWE observado. Gráficos izquierda: Calor latente con y sin corrección atmosférica y el balance de energía neto. Se observa un cambio en el calor latente y por lo tanto en el balance neto. De este cambio se intepreta que se reconstruye más energía con la corrección atmosférica para el derretimiento, para las condiciones locales de la estación. 
</p>


<p align="justify"> 4. Notar que al no considerar flujo energético por precipitación este evento no se ve reflejado en los resultados de los balances, aunque haya un aumento en el SWE que observa la estación. Notar también la oscilación del SWE observado, presumiblemente por las condiciones adversas de la estación y la variación de la temperatura y presión atmosférica a estas altitudes. En cuanto a los resultados oscilantes del balance de energía esto es porque se usaron los datos cada una hora y existe una oscilación producto de la radiación incidente entre el día y la noche. </p>







