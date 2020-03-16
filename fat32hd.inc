     OEM_ID                db 		"CBOS0.01";系统版本号   8个字节
     BytesPerSector        dw 		0x0200;每扇区字节数   2个字节
     SectorsPerCluster     db 		0x01;每簇扇区数 默认 8 1个字节
     ReservedSectors       dw 		0x20;保留扇区个数   2个字节
     ;-------16 bit ⬆️------->
     TotalFATs             db 		0x02;FAT表格数 默认2 1个字节
     MaxRootEntries        dw 		0x00;根目录最多可容纳 目录数 历史字段 FAT32不适用 2个字节
     NumberOfSectors       dw 		0x00;扇区总数 小于32M 在这记录 大于32M 改TotalSectors  2个字节
     MediaDescriptor       db 		0xF8;介质描述 0xF8位硬盘 1个字节
     SectorsPerFAT         dw 		0x00;每个FAT表 的大小扇区数 FAT32 已经不使用了 2个字节
     SectorsPerTrack       dw 		63;每个磁道扇区数 S          ---------->不同硬盘改动 2个字节
     SectorsPerHead        dw 		16;磁头数    H            ---------------->修改 2个字节
     HiddenSectors         dd 		0x00;分区目前已使用 隐藏扇区数 4个字节
     ;-------16 bit ⬆️------->
     TotalSectors     	   dd 		0x11f70;大于32M这里修改 扇区数 ---------------> 4个字节
     BigSectorsPerFAT      dd 		0x0236;每个FAT表使用扇区数  ----------->  4个字节
     Flags                 dw 		0x00;标记 2个字节
     FSVersion             dw 		0x00;版本号 2个字节
     RootDirectoryStart    dd 		0x02;根目录簇号 4个字节 起始于数据区的  + 2个簇 4个字节
     ;-------16 bit ⬆️------->
     FSInfoSector          dw 		0x01;文件系统信息扇区 2个字节
     BackupBootSector      dw 		0x06;备份引导扇区 位于文件系统的6号扇区 2个字节
     TIMES 12              db       0x00;预留12字节未使用
     ;-------16 bit ⬆️------->
     DriveNumber           db 		0x80;驱动器编号 0号设备 1个字节
     ReservedByte          db   	0x00;预留  1个字节
     Signature             db 		0x29;扩展引导标记 1个字节
     VolumeID              dd 		0x00 ;4个字节
     VolumeLabel           db 		"QUASI  BOOT" ;11个字节
     SystemID              db 		"FAT32   ";文件系统 8个字节	