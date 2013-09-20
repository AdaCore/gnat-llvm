# Makefile for JGNAT
# Copyright (C) 2006, AdaCore

TARGET=llvm
TARGETLANG=LLVM
DEF_TARGET=No_VM
LIBDIR=gnat2llvm
NON_ADA_RT_FILES=*.c
objext=.o

TOOLS_PREFIX=llvm-
GNATCOMPILE=llvm-gnatcompile
SHORT_GNATCOMPILE=gnatcompile
GNATCMD=gnat
EXTRA_TOOLS=

GNATRTL_COMMON_SRCS=\
   s-oscons.adb

EXTRA_EXCLUDE_GNATRTL_OBJS= \
   a-assert% s-assert% \
   a-tags% \
   \
   a-stream% \
   a-btgbso% a-chtgbo% a-chtgbk% a-chtgke% a-chtgop% a-cidlli% a-cihama% \
   a-cihase% a-ciorma% a-ciormu% a-ciorse% a-cogeso% a-cohama% a-cohase% \
   a-cohata% a-coinho% a-coinve% a-convec% a-cobove% a-cofove% a-coorma% \
   a-coormu% a-coorse% a-coprnu% a-coteio% a-crbltr% a-crbtgk% a-crbtgo% \
   a-crdlli% a-comutr% a-cimutr% a-csquin% a-cuprqu% a-cusyqu% a-direio% \
   a-envvar% a-except% a-colire% a-einuoc% \
   \
   s-finroo% a-finali% a-flteio% a-fwteio% a-fzteio% a-inteio% a-ioexce% \
   a-iwteio% a-izteio% a-lcteio% a-lfteio% a-lfwtio% a-lfztio% a-liteio% \
   a-liwtio% a-liztio% a-llctio% a-llftio% a-llfwti% a-llfzti% a-llitio% \
   a-lliwti% a-llizti% a-numeri% a-ncelfu% a-ngcefu% a-ngcoar% a-ngcoty% \
   a-ngelfu% a-ngrear% a-nlcefu% a-nlcoar% a-nlcoty% a-nlelfu% a-nlrear% \
   a-nllcar% a-nllcef% a-nllcty% a-nllefu% a-nllrar% a-nscefu% \
   \
   a-cbhama% a-cbhase% a-cborse% a-cbdlli% a-cbmutr% a-cborma% \
   a-cbprqu% a-contai% a-cbsyqu% a-cdlili% a-cfdlli% a-cfhama% a-cfhase% \
   a-cforma% a-cforse% a-cgaaso% a-cgarso% a-cgcaso% \
   \
   a-string% a-chahan% \
   \
   a-calend% a-calari% a-calcon% a-caldel% a-calfor% a-catizo% \
   a-chacon% \
   a-nscoty% \
   a-nselfu% \
   a-nucoar% \
   a-nucoty% \
   a-nudira% \
   a-nuelfu% \
   a-nuflra% \
   a-numaux% \
   a-nurear% \
   a-rbtgbk% \
   a-rbtgbo% \
   a-rbtgso% \
   a-sbecin% \
   a-sbecin% \
   a-sbhcin% \
   a-sbhcin% \
   a-sblcin% \
   a-sblcin% \
   a-scteio% \
   a-secain% \
   a-secain% \
   a-sequio% \
   a-sequio% \
   a-sfecin% \
   a-sfhcin% \
   a-sflcin% \
   a-sfteio% \
   a-sfwtio% \
   a-sfztio% \
   a-shcain% \
   a-shcain% \
   a-siteio% \
   a-siwtio% \
   a-siztio% \
   a-slcain% \
   a-slcain% \
   a-ssicst% \
   a-ssitio% \
   a-ssiwti% \
   a-ssizti% \
   a-stboha% \
   a-stfiha% \
   a-stmaco% \
   a-strbou% \
   a-strfix% \
   a-strfix% \
   a-strhas% \
   a-strmap% \
   a-strsea% \
   a-strsea% \
   a-strsup% \
   a-strsup% \
   a-strunb% \
   a-strunb% \
   a-ststio% \
   a-ststio% \
   a-stunha% \
   a-stuten% \
   a-stwibo% \
   a-stwifi% \
   a-stwifi% \
   a-stwiha% \
   a-stwima% \
   a-stwise% \
   a-stwise% \
   a-stwisu% \
   a-stwisu% \
   a-stwiun% \
   a-stwiun% \
   a-stzbou% \
   a-stzfix% \
   a-stzfix% \
   a-stzhas% \
   a-stzmap% \
   a-stzsea% \
   a-stzsea% \
   a-stzsup% \
   a-stzsup% \
   a-stzunb% \
   a-stzunb% \
   a-suenco% \
   a-suenst% \
   a-suewst% \
   a-suezst% \
   a-suteio% \
   a-swbwha% \
   a-swfwha% \
   a-swmwco% \
   a-swuwha% \
   a-swuwti% \
   a-szbzha% \
   a-szfzha% \
   a-szmzco% \
   a-szuzha% \
   a-szuzti% \
   a-teioed% \
   a-teioed% \
   a-textio% \
   a-tgdico% \
   a-tiboio% \
   a-ticoau% \
   a-ticoio% \
   a-tideau% \
   a-tideio% \
   a-tienau% \
   a-tienau% \
   a-tienio% \
   a-tifiio% \
   a-tiflau% \
   a-tiflio% \
   a-tigeau% \
   a-tiinau% \
   a-tiinio% \
   a-timoau% \
   a-timoio% \
   a-tirsfi% \
   a-titest% \
   a-tiunio% \
   a-witeio% \
   a-wrstfi% \
   a-wtcoau% \
   a-wtcoio% \
   a-wtdeau% \
   a-wtdeio% \
   a-wtedit% \
   a-wtedit% \
   a-wtenau% \
   a-wtenau% \
   a-wtenio% \
   a-wtfiio% \
   a-wtflau% \
   a-wtflio% \
   a-wtgeau% \
   a-wtinau% \
   a-wtinio% \
   a-wtmoau% \
   a-wtmoio% \
   a-wttest% \
   a-wwboio% \
   a-wwunio% \
   a-zrstfi% \
   a-ztcoau% \
   a-ztcoio% \
   a-ztcstr% \
   a-ztdeau% \
   a-ztdeio% \
   a-ztedit% \
   a-ztedit% \
   a-ztenau% \
   a-ztenau% \
   a-ztenio% \
   a-ztexio% \
   a-ztfiio% \
   a-ztflau% \
   a-ztflio% \
   a-ztgeau% \
   a-ztinau% \
   a-ztinio% \
   a-ztmoau% \
   a-ztmoio% \
   a-zttest% \
   a-zzboio% \
   a-zzunio% \
   calendar% \
   directio% \
   g-io_aux% \
   g-mbdira% \
   g-mbflra% \
   g-moreex% \
   g-moreex% \
   g-rannum% \
   g-regexp% \
   g-sercom% \
   ioexcept% \
   s-bitops% \
   s-commun% \
   s-diflio% \
   s-diinio% \
   s-dimkio% \
   s-dimmks% \
   s-direio% \
   s-direio% \
   s-dmotpr% \
   sequenio% \
   s-ficobl% \
   s-fileio% \
   s-fileio% \
   s-finmas% \
   s-finmas% \
   s-gearop% \
   s-gloloc% \
   s-llflex% \
   s-memory% \
   s-ransee% \
   s-regexp% \
   s-regpat% \
   s-rpc% \
   s-secsta% \
   s-sequio% \
   s-shasto% \
   s-soflin% \
   s-stalib% \
   s-stopoo% \
   s-stratt% \
   s-stratt% \
   s-ststop% \
   s-ststop% \
   s-tasloc% \
   s-valwch% \
   text_io% \
   g-regpat% \
   g-tasloc% \
   s-rannum% \
   a-comlin% \
   a-wichha% \
   a-zchhan% \
   g-deutst% \
   g-enutst% \
   i-c% \
   s-addima% \
   s-arit64% \
   s-imgenu% \
   s-valuti% \
   s-wchstw% \
   s-wchwts% \
   s-explli% \
   s-wwdcha% \
   s-multip% \
   s-osprim% \
   s-valboo% \
   s-valcha% \
   s-valenu% \
   s-valint% \
   s-vallli% \
   s-valllu% \
   s-valrea% \
   s-valuns% \
   s-wwdenu% \
   s-wwdwch% \
   s-valdec% \
   s-vallld% \


-include ../stubs/Makefile.common

sanity-check:
ifeq (,$(findstring $(TARGET),$(target)))
	@echo "You must run make in the src/$(TARGET) directory"
	@exit 1
endif
	@if [ ! -f $(GNAT_SRC)/Makefile.rtl ]; then \
	  echo "$(GNAT_SRC)/Makefile.rtl not found"; exit 1; \
	fi
libgnat: $(GNATRTL_OBJS)
