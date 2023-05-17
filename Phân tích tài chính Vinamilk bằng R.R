#----------------------------BẢNG LƯU CHUYỂN TIỀN TỆ----------------------------
data = read.csv("C:/Users/ACER/Documents/Zalo Received Files/CLTT.csv")
CDKT=read.csv("C:/Users/ACER/Documents/Zalo Received Files/CDKT.csv")
BKKD = read.csv("C:/Users/ACER/Documents/Zalo Received Files/BKKD.csv")

dim(data)
colnames(data)
dim(CDKT)
colnames(CDKT)
dim(BKKD)
colnames(BKKD)

summary(data)
options(scipen=999)
install.packages("zoo")
library(zoo)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(forecast)
library(zoo)

#-------------------------------------------------------------------------------------------------
#Tiền xử lý số liệu:
colnames(BKKD)[apply(is.na(BKKD), 2, any)]
colnames(CDKT)[apply(is.na(CDKT), 2, any)]
colnames(data)[apply(is.na(data), 2, any)]


BKKD[is.na(BKKD)]=0
CDKT[is.na(CDKT)]=0
data[is.na(data)]=0

colnames(BKKD)[apply(is.na(BKKD), 2, any)]
colnames(CDKT)[apply(is.na(CDKT), 2, any)]
colnames(data)[apply(is.na(data), 2, any)]

class(BKKD$Quý)
class(BKKD$Năm)

BKKD$ThoiGian=as.yearqtr(paste(BKKD$Năm, BKKD$Quý), format = "%Y %q")
class(BKKD$ThoiGian)
head(BKKD$ThoiGian,5)

#-------------------------------------------------------------------------------------------------

#1. Tính lưu chuyển tiền thuần trong kỳ
cltt = subset(data, Quý == 4)
cltt_thuan = cltt %>% 
  group_by(Năm) %>%
  summarise(sum = sum(LC.trong.ĐT, LC.trong.KD, LC.trong.TC, na.rm = TRUE)/1000000)
cltt_thuan

# 2. So sánh lưu chuyển tiền ở các mục kinh doanh qua các năm, tư và tài chính qua các năm với nhau. 

total = cltt_thuan$sum
data1 = cbind(cltt, total) 

ggplot(data1, aes(x = Năm)) +
  geom_line(aes(y = LC.trong.KD/1000000, color = "Kinh doanh"), size = 1.25) +
  geom_line(aes(y = LC.trong.ĐT/1000000, color = "Đầu tư"), size = 1.25) +
  geom_line(aes(y = cltt$LC.trong.TC/1000000, color = "Tài chính"), size = 1.25) +
  geom_line(aes(y = total, color = "Tổng"), size = 1.25) +
  labs(x = "Năm", y = "Tổng (tỷ VND)",
       title = 'Lưu chuyển tiền ở các hoạt động giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 1)) +
  scale_color_manual(name = '', values = c("lightblue", "lightgreen", "pink", 'red'))

# 3. So sánh tỷ trọng ở các mục

subdata = subset(data,Quý==4)
Năm=2010:2022
KDDTTC=matrix(c(subdata$LC.trong.KD/1000000,
                subdata$LC.trong.ĐT/1000000,
                subdata$LC.trong.TC/1000000),ncol=length(Năm),byrow=TRUE)

tmp = data.frame(t(KDDTTC))

df <- data.frame(
  Year = 2010:2022,
  X1 = tmp$X1,
  X2 = tmp$X2,
  X3 = tmp$X3
)

df_long <- gather(df, key = "variable", value = "value", -Year)

ggplot(df_long, aes(x = Year, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Lượng tiền lưu chuyển trong các hoạt động khác nhau giai đoạn 2010 - 2022",
       x = "Năm",
       y = "Lượng tiền lưu chuyển (đơn vị: tỷ VND)",
       fill = "") +
  scale_fill_manual(values = c("cadetblue", "chartreuse4", '#ECA869'),
                    labels = c("Kinh doanh", "Đầu tư", 'Tài chính')) +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))


#------------------------------BẢNG CÂN ĐỐI KẾ TOÁN-----------------------------

# 1. Tính tổng tài sản ngắn hạn, tổng tài sản dài hạn và tổng tài sản qua các năm. 

PTNH = CDKT$PTKH + CDKT$TTNB + CDKT$PTNHK - CDKT$DPPTKĐ # Phải thu ngắn hạn
HTK1 = CDKT$HTK-CDKT$DPGG # Hàng tồn kho
TSNH = CDKT$Tiền + CDKT$ĐTNH + PTNH + HTK1 + CDKT$TSNHK # Tài sản ngắn hạn
CDKT$TongTSNH=TSNH
TSDH = CDKT$PTDH + CDKT$TSCĐ - CDKT$Khấu.hao + CDKT$BĐS.ĐT
        + CDKT$TSDD +CDKT$ĐTTCDH + CDKT$TSDHK # Tài sản dài hạn
CDKT$TongTSDH=TSDH
TongTS = TSNH + TSDH # Tổng tài sản
CDKT$TongTSQuy=TongTS
head(CDKT[,c("TongTSNH","TongTSDH","TongTSQuy")],5)

# 2. So sánh tỷ trọng của ngắn hạn và dài hạn trong tổng tài sản.

temp = cbind(CDKT, TSNH, TSDH)%>%
  group_by(Năm)%>%
  summarise(NH = sum(TSNH)/1000000, DH = sum(TSDH, na.rm = TRUE)/1000000)

ggplot(temp, aes(x = Năm, y = NH)) +
  geom_col(aes(fill = "NH"), alpha = 1) +
  geom_col(aes(y = NH + DH, fill = "NH + DH"), alpha = 0.5) +
  scale_fill_manual(name = '',
                    values = c("NH" = "blue", "NH + DH" = "red"),
                    labels = c("NH" = "Ngắn hạn", "NH + DH" = "Dài hạn")) +
  labs(title = "Tỷ trọng của tài sản ngắn và dài hạn trong tổng tài sản giai đoạn 2010 - 2022",
       x = "Năm",
       y = "Đơn vị (tỷ VND)") 

# 3. So sánh biến động tài sản qua các năm để hiểu thêm về sự phát triển của công ty

temp1 = cbind(CDKT, TongTS)%>%
  group_by(Năm)%>%
  summarise(TTS = sum(TongTS, na.rm = TRUE)/1000000)

temp1$TTS

ggplot(temp1, aes(x = Năm, y = TTS)) +
  geom_line(color = "deepskyblue4", size = 1) +
  labs(title = "Biến động tài sản giai đoạn 2010 - 2022",
       x = "Thời gian", y = "Tổng tài sản (Tỷ VND)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 1)) +
  scale_y_continuous(limits = c(0, 220000), breaks = seq(0, 220000, by = 20000))

# 4. Biểu đồ thể hiện biến động của các mục nhỏ trong tài sản ngắn hạn

TSNH = cbind(CDKT, PTNH, HTK1)%>%
  group_by(Năm)%>%
  summarise(TIEN = sum(Tiền, na.rm = TRUE)/1000000, 
            DTNH = sum(ĐTNH, na.rm = TRUE)/1000000,
            PTNH = sum(PTNH, na.rm = TRUE)/1000000,
            HTK = sum(HTK1, na.rm = TRUE)/1000000,
            TSNHK = sum(TSNHK, na.rm = TRUE)/1000000)
TSNH
ggplot(TSNH, aes(x = Năm)) +
  geom_smooth(aes(y = TIEN, color = "Line 1"), se = FALSE) +
  geom_smooth(aes(y = DTNH, color = "Line 2"), se = FALSE) +
  geom_smooth(aes(y = PTNH, color = "Line 3"), se = FALSE) +
  geom_smooth(aes(y = HTK, color = "Line 4"), se = FALSE) +
  geom_smooth(aes(y = TSNHK, color = "Line 5"), se = FALSE) +
  geom_text(aes(x = max(Năm)+1.5, y = TIEN[length(TIEN)]-500, label = "Tiền và các khoảng tương đương")) +
  geom_text(aes(x = max(Năm)+0.75, y = DTNH[length(DTNH)]+4000, label = "Đầu tư ngắn hạn")) +
  geom_text(aes(x = max(Năm)+1.3, y = PTNH[length(PTNH)]-1000, label = "Các khoản phải thu ngắn hạn")) +
  geom_text(aes(x = max(Năm)+0.65, y = HTK[length(HTK)]+1500, label = "Hàng tồn kho")) +
  geom_text(aes(x = max(Năm)+1.1, y = TSNHK[length(TSNHK)], label = "Tài sản ngắn hạn khác")) +
  labs(x = "Năm", y = "Các mục nhỏ", title = 'Tài sản ngắn hạn') +
  scale_x_continuous(limits = c(2010, 2025), breaks = seq(2010, 2022, by = 1)) + 
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, by = 10000)) +
  theme(legend.position = "none")

# 5. Cơ cấu nguồn vốn

NO = CDKT$Nợ.NH + CDKT$Nợ.DH
temp2 = cbind(CDKT, NO)%>%
  group_by(Năm)%>%
  summarise(NO = sum(NO, na.rm = TRUE)/1000000, 
            VCP = sum(Vốn.CP, na.rm = TRUE)/1000000)

total = temp2$NO[temp2$Năm == 2022] + temp2$VCP[temp2$Năm == 2022]

no = round(temp2$NO[temp2$Năm == 2022]/total * 100, 2)
vcp   = 100 - no

df <- data.frame(
  category = c("NO", "VCP"),
  value = c(44.86, 55.32)
)


ggplot(df, aes(x = "", y = value, fill = category)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("cadetblue", "chartreuse4"),
                    labels = c("Nợ phải trả", "Vốn chủ sở hữu")) +
  labs(title = "Tỷ trọng giữa nợ phải trả và vốn chủ sỡ hữu năm 2022 (%)",
       fill = "",
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5))

# 6. Biến động trong cơ cấu nguồn vốn qua các năm.

df <- data.frame(
  Year = temp2$Năm,
  NO = temp2$NO,
  VCP = temp2$VCP
)

df_long <- pivot_longer(df, cols = c("NO", "VCP"), names_to = "Variable", values_to = "Value")
ggplot(df_long, aes(x = Year, y = Value, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  labs(title = "Tỷ trọng giữa nợ phải trả và vốn chủ sở hữu",
       x = "Năm",
       y = "Giá trị (Tỷ VND)",
       fill = "") +
  scale_fill_manual(values = c("cadetblue", "chartreuse4"),
                    labels = c("Nợ phải trả", "Vốn chủ sở hữu")) +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))

#---------------------------BÁO CÁO KẾT QUẢ KINH DOANH----------------------------

# 1. So sánh giá vốn bán hàng và dịch vụ qua các năm

data = read.csv("C:/Users/ACER/Documents/Zalo Received Files/BKKD.csv")

temp = data %>%
  group_by(Năm) %>%
  summarise(sum = sum(Vốn, na.rm = TRUE)/1000000)

ggplot(data = temp, aes(x = Năm, y = sum)) +
  geom_col(fill = "deepskyblue4") +
  labs(title = "Giá vốn bán hàng và dịch vụ giai đoạn 2010 - 2022",
       x = "Năm",
       y = "Tổng giá vốn (tỷ VND)") +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1)) +
  scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, by = 10000)) +
  geom_text(aes(label = round(sum)), vjust = -0.5)

# 2. So sánh doanh thu hoạt động kinh doanh, chi phí qua các năm

temp2 = data$DoanhthuBH - data$Giảmtrừ - data$Vốn - data$ChiphíBH - data$ChiphíQLDN
temp3 = data$Giảmtrừ + data$Vốn + data$ChiphíBH + data$ChiphíQLDN

data1 = cbind(data, temp2, temp3) %>%
  group_by(Năm)%>%
  summarise(loinhuan = sum(temp2)/1000000,
            chiphi = sum(temp3)/1000000)
data1

temp4 = data%>%
  group_by(Năm)%>%
  summarise(sum = sum(DoanhthuBH)/1000000)
temp4

tmp = data.frame(cbind(temp4, data1$chiphi))
tmp

ggplot(tmp, aes(x=Năm)) + 
  geom_smooth(aes(y=data1.chiphi, color="Chi phí"), se = FALSE) + 
  geom_smooth(aes(y=sum, color="Doanh thu"), se = FALSE) + 
  labs(x="Năm", y="Tổng (tỷ VND)", color="",
       title = 'Lợi nhuận và chi phí chung giai đoạn 2010 - 2022') + 
  scale_color_manual(values=c("chartreuse4", "cadetblue")) +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))

#3 Tính lợi nhuận từ hoạt động tài chính

LNTC = data$DoanhthuTC - data$ChiphíTC
LNTC

#4 So sánh lợi nhuận sau thuế công ty thu được hàng năm

LNK = data$khac - data$Chiphíkhác
LNBH = round(data1$loinhuan)
LNST = c(3595835916,4166604997,5819454717,6472093617,5997908010,7677375712,9245370495,
         10545161872,9814109826,10085159996,10728728149,10426791805,8872670665)
year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
tmp1 = data.frame(cbind(year, LNST))
ggplot(data = tmp1, aes(x = year, y = LNST/1000000)) +
  geom_col(fill = "deepskyblue4") +
  labs(title = "Lợi nhuận sau thuế giai đoạn 2010 - 2022",
       x = "Năm",
       y = "Lợi nhuận (tỷ VND)") +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1)) +
  #  scale_y_continuous(limits = c(-50000, 20000), breaks = seq(-50000, 20000, by = 10000)) +
  geom_text(aes(label = round(LNST/1000000)), vjust = -0.5)

#-------------------------------------------------------------------------------

# Tính toán các chỉ số quan trọng:
BKKD = read.csv("C:/Users/ACER/Documents/Zalo Received Files/BKKD.csv")
CDKT = read.csv('C:/Users/ACER/Documents/Zalo Received Files/CDKT.csv')
BKKD[is.na(BKKD)]=0
CDKT[is.na(CDKT)]=0

temp2 = BKKD$DoanhthuBH - BKKD$Giảmtrừ - BKKD$Vốn - BKKD$ChiphíBH - BKKD$ChiphíQLDN
temp3 = BKKD$Giảmtrừ + BKKD$Vốn + BKKD$ChiphíBH + BKKD$ChiphíQLDN

BKKD1 = cbind(BKKD, temp2, temp3) %>%
  group_by(Năm)%>%
  summarise(loinhuan = sum(temp2),
            chiphi = sum(temp3))
BKKD1

temp4 = BKKD%>%
  group_by(Năm)%>%
  summarise(sum = sum(DoanhthuBH))

PTNH = CDKT$PTKH + CDKT$TTNB + CDKT$PTNHK - CDKT$DPPTKĐ
CDKT$PhaiThuNH = PTNH
HTK = CDKT$HTK - CDKT$DPGG
CDKT$HangTK = HTK
TSNH = CDKT$Tiền + CDKT$ĐTNH + PTNH + HTK + CDKT$TSNHK
CDKT$TongTSNH = TSNH
TSDH = CDKT$PTDH + CDKT$TSCĐ - CDKT$Khấu.hao + CDKT$BĐS.ĐT 
      + CDKT$TSDD + CDKT$ĐTTCDH + CDKT$TSDHK
CDKT$TongTSDH = TSDH
CDKT$TongTS = TSNH + TSDH
CDKT$NoPT = CDKT$Nợ.DH + CDKT$Nợ.NH
CDKT$Vốn.CSH = CDKT$Vốn.CP + CDKT$TD.Vốn + CDKT$Quỹ + CDKT$LNST
CDKT$NV = CDKT$NoPT + CDKT$Vốn.CSH

HTKBQ=c()
for (i in seq(2, length(HTK))) 
{
  HTKBQ[i-1] = (HTK[i]+ HTK[i-1])/2
  i = i + 1
}
HTKBQ

HTK4=CDKT$HangTK[CDKT$Quý==4]
HTK4
HTKBQN=c()
for (i in seq(2,length(HTK4))) 
{
  HTKBQN[i-1]=(HTK[i] + HTK[i-1])/2
  i = i + 1
}
HTKBQN

temp5 = BKKD$DoanhthuBH - BKKD$Giảmtrừ
BKKD2 = cbind(BKKD, temp5) %>%
  group_by(Năm)%>%
  summarise(doanhthuthuan = sum(temp5))
BKKD2
PTNam=CDKT$PhaiThuNH[CDKT$Quý==4]
PTNam

CDKT$TaiSanCĐ = CDKT$TSCĐ - CDKT$Khấu.hao
TaiSanCĐ1 = CDKT$TaiSanCĐ[CDKT$Quý == 4]
TaiSanCĐ1
TaiSanBQN=c()
for (i in seq(2,length(TaiSanCĐ1))) 
{
  TaiSanBQN[i-1]=(TaiSanCĐ1[i]+TaiSanCĐ1[i-1])/2
  i = i + 1
}
TaiSanBQN

TongTS1=CDKT$TongTS[CDKT$Quý==4]
TongTS1
TongTSBQN=c()
for (i in seq(2, length(TongTS1))) 
{
  TongTSBQN[i-1] = (TongTS1[i] + TongTS1[i-1])/2
  i = i + 1
}
TongTSBQN
BKKD
LNK = BKKD$TNkhác - BKKD$Chiphíkhác
LNK
LNTC = BKKD$DoanhthuTC - BKKD$ChiphíTC
BKKD3 = cbind(BKKD, LNTC, LNK)%>%
  group_by(Năm)%>%
  summarise(lntc = sum(LNTC), lnk = sum(LNK))
BKKD3$lnkd = BKKD1$loinhuan
BKKD3$thuehh = BKKD$ThuếTNDNhh[BKKD$Quý==4]
BKKD3$thuehl = BKKD$ThuếTNDNhoãn[BKKD$Quý==4]
BKKD3
BKKD3$lnsauthue = BKKD3$lntc + BKKD3$lnk + BKKD3$lnkd - BKKD3$thuehh - BKKD3$thuehl
BKKD3$lnsauthue = c(3595835916,4166604997,5819454717,6472093617,5997908010,7677375712,9245370495,
                    10545161872,9814109826,10085159996,10728728149,10426791805,8872670665)

dim(CDKT)
colnames(CDKT)
#Trung bình doanh thu bán hàng và cung cấp dịch vụ:

mean(temp4$sum)
median(temp4$sum)

# Kiểm định 1-sample test t-test với giả thiết trung bình và trung vị của doanh thu bằng nhau

t.test(temp4$sum, mu = median(temp4$sum))

# Chỉ số thanh toán
####1. Thanh toán tổng quát
CDKT$HeSoTTTQ=CDKT$TongTS/CDKT$NoPT
CDKT$HeSoTTTQ

HeSoTTTQnam=CDKT$HeSoTTTQ[CDKT$Quý==4]
HeSoTTTQnam = data.frame(year = c(2010:2022), HeSoTTTQnam)
HeSoTTTQnam

ggplot(HeSoTTTQnam, aes(x = year)) +
  geom_line(aes(y = HeSoTTTQnam), color = '#1f4484', size = 1.25) + 
  labs(x="Năm", y="Hệ số",
       title = 'Hệ số thanh toán tổng quát năm giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))


####2. Thanh toán hiện hành:
CDKT$HeSoTTHH=TSNH/CDKT$Nợ.NH

HeSoTTHHnam=CDKT$HeSoTTHH[CDKT$Quý==4]
HeSoTTHHnam
HeSoTTHHnam = data.frame(year = c(2010:2022), heso = HeSoTTHHnam)
ggplot(HeSoTTHHnam, aes(x = year)) +
  geom_line(aes(y = heso), color = '#1f4484', size = 1.25) + 
  labs(x="Năm", y="Hệ số",
       title = 'Hệ số thanh toán hiện hàng năm giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))


####3. Thanh toán nhanh:
CDKT$HeSoTTN=(TSNH-HTK)/CDKT$Nợ.NH

HeSoTTNnam=CDKT$HeSoTTN[CDKT$Quý==4]
#HeSoTTNnam = data.frame(x = HeSoTTNnam)
hist(HeSoTTNnam, main = "Biểu đồ histogram của Hệ số thanh toán nhanh", 
     xlab = "Giá trị", ylab = "Tần số", breaks = 4, col = "#1f4484")


####4. Thanh toán tức thời:
CDKT$HeSoTTTT=CDKT$Tiền/CDKT$Nợ.NH

HeSoTTTTnam=CDKT$HeSoTTTT[CDKT$Quý==4]
HeSoTTTTnam

ggplot(data.frame(year = c(2010:2022), HeSoTTTTnam), aes(x= year)) +
  geom_line(aes(y = HeSoTTTTnam), color = '#1f4484', size = 1.25) +
  labs(x="Năm", y="Hệ số",
       title = 'Hệ số thanh toán hiện tức thời giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))


#----------------------------------------------------------------
###Chỉ số nhóm đòn bẩy tài chính:
####1. Hệ số nợ:
CDKT$HeSoNo=CDKT$NoPT/CDKT$TongTS

HeSoNonam=CDKT$HeSoNo[CDKT$Quý==4]
HeSoNonam

ggplot(data.frame(year = c(2010:2022), HeSoNonam), aes(x= year)) +
  geom_line(aes(y = HeSoNonam), color = '#1f4484', size = 1.25) +
  labs(x="Năm", y="Hệ số",
       title = 'Hệ số nợ năm giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))

####2. Hệ số nợ trên Vốn chủ sở hữu:
CDKT$HeSoNoVCSH=CDKT$NoPT/CDKT$Vốn.CSH

HeSoNoVCSHnam=CDKT$HeSoNoVCSH[CDKT$Quý==4]
HeSoNoVCSHnam

ggplot(data.frame(year = c(2010:2022), HeSoNoVCSHnam), aes(x= year)) +
  geom_line(aes(y = HeSoNoVCSHnam), color = '#1f4484', size = 1.25) +
  labs(x="Năm", y="Hệ số",
       title = 'Hệ số nợ trên vốn chủ sở hữu giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))



####3. Hệ số tự tài trợ:
CDKT$HeSoTuTT=CDKT$Vốn.CSH/CDKT$NV

HeSoTuTTnam=CDKT$HeSoTuTT[CDKT$Quý==4]
HeSoTuTTnam
max(HeSoTuTTnam)
min(HeSoTuTTnam)


#--------------------------------------------------------------
###Chỉ số hiệu quả quản lý tài sản:
####1. Vòng quay hàng tồn kho:
Von=BKKD$Vốn[-1]
VQHTK=Von/HTKBQ
VQHTK

####2. Thời gian tồn kho bình quân:
TGTK=90/VQHTK
TGTK

####3. Kỳ thu tiền bình quân:
KTTBQ=(PTNam*365)/BKKD2$doanhthuthuan
KTTBQ

####4. Hiệu suất sử dụng tài sản cố định:
DTThuan=BKKD2$doanhthuthuan[-1]
HSSDTSCĐ=DTThuan/TaiSanBQN
HSSDTSCĐ

####5. Vòng quay tổng tài sản:
VQTTS=DTThuan/TongTSBQN
VQTTS

#------------------------------------------------------------
###Chỉ số về khả năng sinh lời:
####1. Tỷ suất lợi nhuận trên doanh thu
TSLNhuanDThu=BKKD3$lnsauthue/BKKD2$doanhthuthuan*100
TSLNhuanDThu

ggplot(data.frame(year = c(2010:2022), TSLNhuanDThu), aes(x= year)) +
  geom_line(aes(y = TSLNhuanDThu), color = '#1f4484', size = 1.25) +
  labs(x="Năm", y="Tỷ suất (%)",
       title = 'Tỷ suất lợi nhuận trên doanh thu giai đoạn 2010 - 2022') +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2010, 2022, by = 1))

####2. Tỷ suất lợi nhuận trên tổng tài sản (ROA):
LNsauthue=BKKD3$lnsauthue[-1]
ROA=LNsauthue/TongTSBQN
ROA
ROA_DutchLady=c(0.0693, 0.0988, 0.0969, 0.0661, 0.0801, 0.0852, 0.0705, 0.0617, 0.0559, 0.0506, 0.0327, 0.0392)

ggplot(data.frame(year = c(2011:2022), roa = ROA, road = ROA_DutchLady),
       aes(x = year)) +
  geom_line(aes(y = roa, color = "ROA"), size = 1.25) +
  geom_line(aes(y = road, color = "DutchLady ROA"), size = 1.25) +
  labs(x="Năm", y="Tỷ suất (%)",
       title = 'Tỷ suất lợi nhuận trên tổng tài sản (ROA) giai đoạn 2011 - 2022') +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2011, 2022, by = 1)) +
  scale_color_manual(name = "",
                     values = c("ROA" = "#1f4484", "DutchLady ROA" = "chartreuse4"))


#-------------------------------------------------------------------------
#Dự đoán doanh thu bán hàng bằng mô hình ARIMA:

# Đọc dữ liệu

df <- read.csv("C:/Users/ACER/Documents/Zalo Received Files/BKKD.csv")
data = df$DoanhthuBH
data
#---------------------------

plot.ts(data)
adf.test(data)

# Tính sai phân bậc một
diff_data <- diff(data, differences = 1)
plot.ts(diff_data, main="Sai phân bậc 1")

# Kiểm định ADF cho sai phân bậc một
adf.test(diff_data)

# Xây dựng mô hình
auto.arima(train)
fit <- arima(train, order = c(0,1,0))
summary(fit)

#Kiểm tra mô hình
et = residuals(fit)
et
acf(et)
Box.test(et, lag = 10, type = c('Box-Pierce', 'Ljung-Box'), fitdf = 1)

# Dự đoán giá trị

train <- window(data, end = c(38))
test <- window(data, start = c(39))
fc <- forecast(fit, h = length(test))
pred <- fc$mean


# Tính toán độ chính xác dựa trên MAPE
MAPE <- 100 * mean(abs((test - pred) / test))
MAPE
accuracy_rate <- 100 - MAPE
accuracy_rate

# Dự đoán giá trị của 3 năm tiếp theo theo quý:
forecast_values <- forecast(fit, h = 12)
forecast_values

plot(fc)
