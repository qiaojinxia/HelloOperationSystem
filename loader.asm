org 10000h
	mov ax,cs
	mov ds,ax
	mov es,ax
	mov ax,0x00
	mov ss,ax
	mov sp,0x7c00
;========== 显示 Start Loader....
	mov ax,1301h
	mov bx,000fh
	mov dx,0200h
	mov cx,0x6D6
	push ax
	mov ax,ds
	mov es,ax
	pop ax
	mov bp,StartLoaderMessage
	int 10h
	jmp $
;=============
StartLoaderMessage : db "Robert Redfield, director of the Centers for Disease Control and Prevention (CDC), was asked by lawmakers when testifying in a House subcommittee on Wednesday if it’s possible thatsome Americans died of what on the surface seems like influenza but is in fact COVID-19. He confirmed it.This is a very important piece of information, telling people that the state of novel coronavirus outbreak in the US is much larger than the data that has been released so far. The US just reported over one thousand of confirmed cases and more than 30 deaths. The US’ testing capacity has been far from enough to meet the demand, leading to a cover-up of the true state of the epidemic.Several celebrities in the US have contracted the coronavirus, military personnel and congressional staff have been infected, and somepoliticians have been in close contact of confirmed patients. This reflects how the epidemic in the US is widespread.President Trump said the US is more prepared than any other country to deal with the new coronavirus, and the virus will not have a chance in the US. But his promises have done little to reassure. The US hasn’t taken firm steps to expand the scope of testing, or to significantly reduce human contact. That means not only are there more infected people in the US than official number, those infected people have many opportunities to contact healthy people. In the US, efforts to stimulate the economy seem to be moving faster than efforts to prevent epidemic, and protecting people’s lives still doesn't seem to be a top federal policy priority. But with investors lacking confidence in the US response, the stock market in the US has fallen sharply and the markets are filled with pessimism and panic. "	
