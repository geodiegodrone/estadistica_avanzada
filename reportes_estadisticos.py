import pandas as pd
from pandas_profiling import ProfileReport

#Cargar los datos
datos = pd.read_csv('D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/actividad_1/datos_rstudio/PRSA_Data_20130301-20170228/ANM_Producci_n_Nacional_de_Minerales_y_Contraprestaciones_Econ_micas_Trimestral.csv')
datos
datos_filtrados = datos.loc[datos['Recurso Natural'] == 'ORO']
datos_filtrados

#Generar reporte
reporte = ProfileReport(datos_filtrados, title = 'Reporte de datos de la ANM')

#Ver datos
reporte

#Exportar reporte a html
reporte.to_file('reporte_datos_anm.html')

# exportar los datos con la nueva columna a un nuevo archivo CSV
datos_filtrados.to_csv('ANM_Producci_n_Nacional_de_Minerales_y_Contraprestaciones_Econ_micas_Trimestral_oro.csv', index=False)
