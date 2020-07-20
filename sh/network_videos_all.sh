#!/bin/bash
date=2020-07-12
dateini=2020-02-01

../../sh/network_anim.sh Brasil_Municip_Network_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Nordeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Nordeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Nordeste_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_NordesteLitoral_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_NordesteLitoral_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_NordesteLitoral_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_SP_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_SP_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_SP_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_RMSP_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_RMSP_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_RMSP_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Centro-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Centro-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Centro-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Sul-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Sul-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Sul-Sudeste_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_RMRJ_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_RMRJ_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_RMRJ_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_RMGO_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_RMGO_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_RMGO_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_RMCTBA_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_RMCTBA_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_RMCTBA_ $dateini $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_CE_RN_PB_PE_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_CE_RN_PB_PE_ $dateini $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_CE_RN_PB_PE_ $dateini $date || { echo 'falhou' ; exit 1; }
