#!/bin/bash
date=2020-06-03

../../sh/network_anim.sh Brasil_Municip_Network_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Nordeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Nordeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Nordeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_SP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_SP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_SP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_RMSP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_RMSP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_RMSP_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Centro-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Centro-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Centro-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

../../sh/network_anim.sh Brasil_Municip_Network_Zoom_Sul-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Flux_Zoom_Sul-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }
../../sh/network_anim.sh Brasil_Municip_Network_Iso_Zoom_Sul-Sudeste_ 2020-03-01 $date || { echo 'falhou' ; exit 1; }

