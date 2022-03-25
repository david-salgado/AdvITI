provinciaToCCAA <- function(provincia){
  
  CA1 = c('04','11','14','18','21','23','29','41')
  CA2 = c('22','44','50')
  CA3 = c('33')
  CA4 = c('07')
  CA5 = c('35','38')
  CA6 = c('39')
  CA7 = c('05','09','24','34','37','40','42','47','49')
  CA8 = c('02','13','16','19','45')
  CA9 = c('08','17','25','43')
  CA10 = c('03','12','46')
  CA11 = c('06','10')
  CA12 = c ('15','27','32','36')
  CA13 = c('28')
  CA14 = c('30')
  CA15 = c('31')
  CA16 = c('01','20','48')
  CA17 = c('26')
  CA18 = c('51')
  CA19 = c('52')
  
  auxDT <- data.table(provincia = provincia)[
    provincia %in% CA1, comunidad := '01'][
      provincia %in% CA2,comunidad  := '02'][
        provincia %in% CA3, comunidad := '03'][
          provincia %in% CA4, comunidad := '04'][
            provincia %in% CA5, comunidad := '05'][
              provincia %in% CA6,comunidad  := '06'][
                provincia %in% CA7, comunidad := '07'][
                  provincia %in% CA8, comunidad := '08'][
                    provincia %in% CA9, comunidad := '09'][
                      provincia %in% CA10,comunidad  := '10'][
                        provincia %in% CA11, comunidad := '11'][
                          provincia %in% CA12, comunidad := '12'][
                            provincia %in% CA13, comunidad := '13'][
                              provincia %in% CA14,comunidad  := '14'][
                                provincia %in% CA15, comunidad := '15'][
                                  provincia %in% CA16, comunidad := '16'][
                                    provincia %in% CA17, comunidad := '17'][
                                      provincia %in% CA18,comunidad  := '18'][
                                        provincia %in% CA19, comunidad := '19']
  
  auxDT[is.na(comunidad) | provincia == '', provincia := '99']
  comunidad <- auxDT$comunidad
  return(comunidad)
  
}
