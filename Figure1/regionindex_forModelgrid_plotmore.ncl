load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

   ony        = 143
   onx        = 144

   ny         = 143
   nx         = 144

     filename11     = "../mask_lmdz_koppen.nc"
     lmdzm10        = addfile(filename11,"r")
     latR           = lmdzm10->lat
     lonR           = lmdzm10->lon
     values_2d      = lmdzm10->Band1

   do i = 0,ny-1
     do j= 0,nx-1
      if(values_2d(i,j).eq.44).and.(lonR(j).le.90)then
         values_2d(i,j) = 0
      end if
      if(values_2d(i,j).eq.47).and.(latR(i).le.37)then
         values_2d(i,j) = 0
      end if

      if(values_2d(i,j).eq.40)then
         values_2d(i,j) = 29
      end if
      if(values_2d(i,j).eq.12)then
         values_2d(i,j) = 29
      end if

      if(values_2d(i,j).eq.29).and.(latR(i).le.30).and.(lonR(i).le.-112.5)then
         values_2d(i,j) = 0
      end if
      if(values_2d(i,j).eq.29).and.(latR(i).le.28).and.(lonR(i).gt.-112.5)then
         values_2d(i,j) = 0
      end if

      if(values_2d(i,j).eq.1)then
         values_2d(i,j) = 15
      end if

      if(values_2d(i,j).eq.37).and.(lonR(j).ge.22).and.(lonR(j).le.35).and.(latR(i).ge.15).and.(latR(i).le.30)then
         values_2d(i,j) = 60
      end if
      if(values_2d(i,j).eq.37).and.(lonR(j).ge.35).and.(lonR(j).le.60).and.(latR(i).ge.12).and.(latR(i).le.25)then
         values_2d(i,j) = 60
      end if

     end do
   end do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print("---Reading LAT anf LON  And models name-----")

     lmdzm10        = addfile("/bdd/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/amip/r1i1p1f1/Lmon/mrsos/gr/latest/mrsos_Lmon_IPSL-CM6A-LR_amip_r1i1p1f1_gr_201501-201712.nc","r")
     R1sm           = lmdzm10->mrsos(0,:,:)
     R1sm           = lonFlip(R1sm)
     olat            = lmdzm10->lat
     olon            = (/R1sm&lon/)
    delete(R1sm)

   values_2D_Had       = new((/ony,onx/),"integer")
   values_2D_Had       = 0

i_olat = ind_nearest_coord (olat(9:137), latR, 0)
i_olon = ind_nearest_coord (olon, lonR, 0)

;  print("i="+i_olat+"  olat="+olat(9:137)+"  latR(i)="+latR(i_olat))
;  print("---------------------------------------------------")
;  print("i="+i_olon+"  olon="+olon+"  lonR(i)="+lonR(i_olon))

  do i = 9,137
   ii = i_olat(i-9)
    if(olat(i).ge.latR(0)).and.(olat(i).le.latR(ny-1))then
     do j = 0,onx-1
      jj = i_olon(j)
    if(olon(j).ge.lonR(0)).and.(olon(j).le.lonR(nx-1))then
 ;     print("olat = "+olat(i)+"  latR = "+latR(ii))
 ;     print("olon = "+olon(j)+"  lonR = "+lonR(jj))
      if(values_2d(ii,jj).ne.0)then
 ;     print("Region name "+values_2d(ii,jj))
      values_2D_Had(i,j)  = toint(values_2d(ii,jj))
      end if
    end if
     end do
   end if
  end do

  filename11  = "/data/yzhao/CMIP6/mask_koppen_AMIP_grid_11RG.bin"
  system("rm -f " + filename11)
  fbindirwrite(filename11,values_2D_Had)

  RGname_1d = asciiread("../region_3.txt",-1,"string")

RR = (/29,6,16,15,21,43,47,44,23,53,10/)

 Rlat = new((/11,500/),"double")
 Rlon = new((/11,500/),"double")
;10 Amazonia;60 Egypt and Arabia

 do ik = 0,10
   in = 0
   do i = 0,ony-1
     do j= 0,onx-1
      if(values_2D_Had(i,j).eq.RR(ik))then
;             if(ik.eq.10)then
;           print(values_2D_Had(i,j))
;             end if
        Rlat(ik,in)  = olat(i)
        Rlon(ik,in)  = olon(j)
        in = in+1
      end if
         end do
     end do
   end do

  resources                        = True
  resources@gsnMaximize            = True
  resources@gsnFrame               = False                  ; don't advance frame

  colors = (/2,3,4,5,6,7,8,12,13,14,19/)

  wks = gsn_open_wks("pdf","./figure/regions_koppen_ModelGRID_11RG")
  gsn_define_colormap(wks,"default")

  resources@mpMinLonF        = -140                   ; select a subregion
  resources@mpMaxLonF        = 160
  resources@mpMinLatF        = -50
  resources@mpMaxLatF        = 70
  resources@mpCenterLonF     = 0.5*(resources@mpMinLonF + resources@mpMaxLonF)
  resources@mpFillColors = (/"transparent","transparent","lightgray","transparent"/)    ;assign light gray to land masses

  map = gsn_csm_map(wks,resources)

  gsres               = True
  gsres@gsMarkerIndex = 16          ; Use filled dots for markers.
  gsres@gsMarkerSizeF      = 0.003
  gsres@gsMarkerThicknessF = 0.7

 txres               = True
 txres@txFontHeightF = 0.01            ; Set the font height
 txres@txJust        = "centerleft"

  xleg = (/0.07,0.07,0.07,0.3,0.3,0.3,0.53,0.53,0.53,0.76,0.76,0.76,0.87,0.87,0.87/)   ; Location of
  xleg = xleg - 0.05
  xtxt = xleg + 0.015
  yleg = (/0.22,0.17,0.12,0.22,0.17,0.12,0.22,0.17,0.12,0.22,0.17,0.12,0.22,0.17/)   ; and text
  yleg = yleg + 0.03
  ytxt = yleg

  gsres1               = True
  gsres1@gsMarkerIndex = 16
  gsres1@gsMarkerSizeF      = 0.01
  gsres1@gsMarkerThicknessF = 10

   do i= 0,10
     if (.not.ismissing(Rlat(i,0)))then
      gsres@gsMarkerColor      = colors(i)
      gsn_polymarker(wks,map,Rlon(i,:),Rlat(i,:),gsres)
; Add marker and text for the legend.
      gsres1@gsMarkerColor      = colors(i)
      gsn_polymarker_ndc(wks,          xleg(i),yleg(i),gsres1)
      gsn_text_ndc      (wks,RGname_1d(i),xtxt(i),ytxt(i),txres)
    end if
   end do

frame(wks)

end

