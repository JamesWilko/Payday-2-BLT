#include <windows.h>
#include "IPHLPAPI\InitState.h"

#pragma pack(1)


HINSTANCE hLThis = 0;
HINSTANCE hL = 0;
FARPROC p[267] = {0};

BOOL WINAPI DllMain(HINSTANCE hInst,DWORD reason,LPVOID)
	{

	if (reason == DLL_PROCESS_ATTACH)
		{
		hLThis = hInst;
		hL = LoadLibrary("C:\\Windows\\SysWow64\\IPHLPAPI.dll");
		if (!hL) return false;


		p[0] = GetProcAddress(hL,"AddIPAddress");
		p[1] = GetProcAddress(hL,"AllocateAndGetInterfaceInfoFromStack");
		p[2] = GetProcAddress(hL,"AllocateAndGetIpAddrTableFromStack");
		p[3] = GetProcAddress(hL,"CancelIPChangeNotify");
		p[4] = GetProcAddress(hL,"CancelMibChangeNotify2");
		p[5] = GetProcAddress(hL,"CloseCompartment");
		p[6] = GetProcAddress(hL,"CloseGetIPPhysicalInterfaceForDestination");
		p[7] = GetProcAddress(hL,"ConvertCompartmentGuidToId");
		p[8] = GetProcAddress(hL,"ConvertCompartmentIdToGuid");
		p[9] = GetProcAddress(hL,"ConvertGuidToStringA");
		p[10] = GetProcAddress(hL,"ConvertGuidToStringW");
		p[11] = GetProcAddress(hL,"ConvertInterfaceAliasToLuid");
		p[12] = GetProcAddress(hL,"ConvertInterfaceGuidToLuid");
		p[13] = GetProcAddress(hL,"ConvertInterfaceIndexToLuid");
		p[14] = GetProcAddress(hL,"ConvertInterfaceLuidToAlias");
		p[15] = GetProcAddress(hL,"ConvertInterfaceLuidToGuid");
		p[16] = GetProcAddress(hL,"ConvertInterfaceLuidToIndex");
		p[17] = GetProcAddress(hL,"ConvertInterfaceLuidToNameA");
		p[18] = GetProcAddress(hL,"ConvertInterfaceLuidToNameW");
		p[19] = GetProcAddress(hL,"ConvertInterfaceNameToLuidA");
		p[20] = GetProcAddress(hL,"ConvertInterfaceNameToLuidW");
		p[21] = GetProcAddress(hL,"ConvertInterfacePhysicalAddressToLuid");
		p[22] = GetProcAddress(hL,"ConvertIpv4MaskToLength");
		p[23] = GetProcAddress(hL,"ConvertLengthToIpv4Mask");
		p[24] = GetProcAddress(hL,"ConvertRemoteInterfaceAliasToLuid");
		p[25] = GetProcAddress(hL,"ConvertRemoteInterfaceGuidToLuid");
		p[26] = GetProcAddress(hL,"ConvertRemoteInterfaceIndexToLuid");
		p[27] = GetProcAddress(hL,"ConvertRemoteInterfaceLuidToAlias");
		p[28] = GetProcAddress(hL,"ConvertRemoteInterfaceLuidToGuid");
		p[29] = GetProcAddress(hL,"ConvertRemoteInterfaceLuidToIndex");
		p[30] = GetProcAddress(hL,"ConvertStringToGuidA");
		p[31] = GetProcAddress(hL,"ConvertStringToGuidW");
		p[32] = GetProcAddress(hL,"ConvertStringToInterfacePhysicalAddress");
		p[33] = GetProcAddress(hL,"CreateAnycastIpAddressEntry");
		p[34] = GetProcAddress(hL,"CreateIpForwardEntry");
		p[35] = GetProcAddress(hL,"CreateIpForwardEntry2");
		p[36] = GetProcAddress(hL,"CreateIpNetEntry");
		p[37] = GetProcAddress(hL,"CreateIpNetEntry2");
		p[38] = GetProcAddress(hL,"CreatePersistentTcpPortReservation");
		p[39] = GetProcAddress(hL,"CreatePersistentUdpPortReservation");
		p[40] = GetProcAddress(hL,"CreateProxyArpEntry");
		p[41] = GetProcAddress(hL,"CreateSortedAddressPairs");
		p[42] = GetProcAddress(hL,"CreateUnicastIpAddressEntry");
		p[43] = GetProcAddress(hL,"DeleteAnycastIpAddressEntry");
		p[44] = GetProcAddress(hL,"DeleteIPAddress");
		p[45] = GetProcAddress(hL,"DeleteIpForwardEntry");
		p[46] = GetProcAddress(hL,"DeleteIpForwardEntry2");
		p[47] = GetProcAddress(hL,"DeleteIpNetEntry");
		p[48] = GetProcAddress(hL,"DeleteIpNetEntry2");
		p[49] = GetProcAddress(hL,"DeletePersistentTcpPortReservation");
		p[50] = GetProcAddress(hL,"DeletePersistentUdpPortReservation");
		p[51] = GetProcAddress(hL,"DeleteProxyArpEntry");
		p[52] = GetProcAddress(hL,"DeleteUnicastIpAddressEntry");
		p[53] = GetProcAddress(hL,"DisableMediaSense");
		p[54] = GetProcAddress(hL,"EnableRouter");
		p[55] = GetProcAddress(hL,"FlushIpNetTable");
		p[56] = GetProcAddress(hL,"FlushIpNetTable2");
		p[57] = GetProcAddress(hL,"FlushIpPathTable");
		p[58] = GetProcAddress(hL,"FreeMibTable");
		p[59] = GetProcAddress(hL,"GetAdapterIndex");
		p[60] = GetProcAddress(hL,"GetAdapterOrderMap");
		p[61] = GetProcAddress(hL,"GetAdaptersAddresses");
		p[62] = GetProcAddress(hL,"GetAdaptersInfo");
		p[63] = GetProcAddress(hL,"GetAnycastIpAddressEntry");
		p[64] = GetProcAddress(hL,"GetAnycastIpAddressTable");
		p[65] = GetProcAddress(hL,"GetBestInterface");
		p[66] = GetProcAddress(hL,"GetBestInterfaceEx");
		p[67] = GetProcAddress(hL,"GetBestRoute");
		p[68] = GetProcAddress(hL,"GetBestRoute2");
		p[69] = GetProcAddress(hL,"GetCurrentThreadCompartmentId");
		p[70] = GetProcAddress(hL,"GetExtendedTcpTable");
		p[71] = GetProcAddress(hL,"GetExtendedUdpTable");
		p[72] = GetProcAddress(hL,"GetFriendlyIfIndex");
		p[73] = GetProcAddress(hL,"GetIcmpStatistics");
		p[74] = GetProcAddress(hL,"GetIcmpStatisticsEx");
		p[75] = GetProcAddress(hL,"GetIfEntry");
		p[76] = GetProcAddress(hL,"GetIfEntry2");
		p[77] = GetProcAddress(hL,"GetIfStackTable");
		p[78] = GetProcAddress(hL,"GetIfTable");
		p[79] = GetProcAddress(hL,"GetIfTable2");
		p[80] = GetProcAddress(hL,"GetIfTable2Ex");
		p[81] = GetProcAddress(hL,"GetInterfaceInfo");
		p[82] = GetProcAddress(hL,"GetInvertedIfStackTable");
		p[83] = GetProcAddress(hL,"GetIpAddrTable");
		p[84] = GetProcAddress(hL,"GetIpErrorString");
		p[85] = GetProcAddress(hL,"GetIpForwardEntry2");
		p[86] = GetProcAddress(hL,"GetIpForwardTable");
		p[87] = GetProcAddress(hL,"GetIpForwardTable2");
		p[88] = GetProcAddress(hL,"GetIpInterfaceEntry");
		p[89] = GetProcAddress(hL,"GetIpInterfaceTable");
		p[90] = GetProcAddress(hL,"GetIpNetEntry2");
		p[91] = GetProcAddress(hL,"GetIpNetTable");
		p[92] = GetProcAddress(hL,"GetIpNetTable2");
		p[93] = GetProcAddress(hL,"GetIpNetworkConnectionBandwidthEstimates");
		p[94] = GetProcAddress(hL,"GetIpPathEntry");
		p[95] = GetProcAddress(hL,"GetIpPathTable");
		p[96] = GetProcAddress(hL,"GetIpStatistics");
		p[97] = GetProcAddress(hL,"GetIpStatisticsEx");
		p[98] = GetProcAddress(hL,"GetMulticastIpAddressEntry");
		p[99] = GetProcAddress(hL,"GetMulticastIpAddressTable");
		p[100] = GetProcAddress(hL,"GetNetworkInformation");
		p[101] = GetProcAddress(hL,"GetNetworkParams");
		p[102] = GetProcAddress(hL,"GetNumberOfInterfaces");
		p[103] = GetProcAddress(hL,"GetOwnerModuleFromPidAndInfo");
		p[104] = GetProcAddress(hL,"GetOwnerModuleFromTcp6Entry");
		p[105] = GetProcAddress(hL,"GetOwnerModuleFromTcpEntry");
		p[106] = GetProcAddress(hL,"GetOwnerModuleFromUdp6Entry");
		p[107] = GetProcAddress(hL,"GetOwnerModuleFromUdpEntry");
		p[108] = GetProcAddress(hL,"GetPerAdapterInfo");
		p[109] = GetProcAddress(hL,"GetPerTcp6ConnectionEStats");
		p[110] = GetProcAddress(hL,"GetPerTcp6ConnectionStats");
		p[111] = GetProcAddress(hL,"GetPerTcpConnectionEStats");
		p[112] = GetProcAddress(hL,"GetPerTcpConnectionStats");
		p[113] = GetProcAddress(hL,"GetRTTAndHopCount");
		p[114] = GetProcAddress(hL,"GetSessionCompartmentId");
		p[115] = GetProcAddress(hL,"GetTcp6Table");
		p[116] = GetProcAddress(hL,"GetTcp6Table2");
		p[117] = GetProcAddress(hL,"GetTcpStatistics");
		p[118] = GetProcAddress(hL,"GetTcpStatisticsEx");
		p[119] = GetProcAddress(hL,"GetTcpTable");
		p[120] = GetProcAddress(hL,"GetTcpTable2");
		p[121] = GetProcAddress(hL,"GetTeredoPort");
		p[122] = GetProcAddress(hL,"GetUdp6Table");
		p[123] = GetProcAddress(hL,"GetUdpStatistics");
		p[124] = GetProcAddress(hL,"GetUdpStatisticsEx");
		p[125] = GetProcAddress(hL,"GetUdpTable");
		p[126] = GetProcAddress(hL,"GetUniDirectionalAdapterInfo");
		p[127] = GetProcAddress(hL,"GetUnicastIpAddressEntry");
		p[128] = GetProcAddress(hL,"GetUnicastIpAddressTable");
		p[129] = GetProcAddress(hL,"Icmp6CreateFile");
		p[130] = GetProcAddress(hL,"Icmp6ParseReplies");
		p[131] = GetProcAddress(hL,"Icmp6SendEcho2");
		p[132] = GetProcAddress(hL,"IcmpCloseHandle");
		p[133] = GetProcAddress(hL,"IcmpCreateFile");
		p[134] = GetProcAddress(hL,"IcmpParseReplies");
		p[135] = GetProcAddress(hL,"IcmpSendEcho");
		p[136] = GetProcAddress(hL,"IcmpSendEcho2");
		p[137] = GetProcAddress(hL,"IcmpSendEcho2Ex");
		p[138] = GetProcAddress(hL,"InitializeIpForwardEntry");
		p[139] = GetProcAddress(hL,"InitializeIpInterfaceEntry");
		p[140] = GetProcAddress(hL,"InitializeUnicastIpAddressEntry");
		p[141] = GetProcAddress(hL,"InternalCleanupPersistentStore");
		p[142] = GetProcAddress(hL,"InternalCreateAnycastIpAddressEntry");
		p[143] = GetProcAddress(hL,"InternalCreateIpForwardEntry");
		p[144] = GetProcAddress(hL,"InternalCreateIpForwardEntry2");
		p[145] = GetProcAddress(hL,"InternalCreateIpNetEntry");
		p[146] = GetProcAddress(hL,"InternalCreateIpNetEntry2");
		p[147] = GetProcAddress(hL,"InternalCreateUnicastIpAddressEntry");
		p[148] = GetProcAddress(hL,"InternalDeleteAnycastIpAddressEntry");
		p[149] = GetProcAddress(hL,"InternalDeleteIpForwardEntry");
		p[150] = GetProcAddress(hL,"InternalDeleteIpForwardEntry2");
		p[151] = GetProcAddress(hL,"InternalDeleteIpNetEntry");
		p[152] = GetProcAddress(hL,"InternalDeleteIpNetEntry2");
		p[153] = GetProcAddress(hL,"InternalDeleteUnicastIpAddressEntry");
		p[154] = GetProcAddress(hL,"InternalFindInterfaceByAddress");
		p[155] = GetProcAddress(hL,"InternalGetAnycastIpAddressEntry");
		p[156] = GetProcAddress(hL,"InternalGetAnycastIpAddressTable");
		p[157] = GetProcAddress(hL,"InternalGetForwardIpTable2");
		p[158] = GetProcAddress(hL,"InternalGetIPPhysicalInterfaceForDestination");
		p[159] = GetProcAddress(hL,"InternalGetIfEntry2");
		p[160] = GetProcAddress(hL,"InternalGetIfTable");
		p[161] = GetProcAddress(hL,"InternalGetIfTable2");
		p[162] = GetProcAddress(hL,"InternalGetIpAddrTable");
		p[163] = GetProcAddress(hL,"InternalGetIpForwardEntry2");
		p[164] = GetProcAddress(hL,"InternalGetIpForwardTable");
		p[165] = GetProcAddress(hL,"InternalGetIpInterfaceEntry");
		p[166] = GetProcAddress(hL,"InternalGetIpInterfaceTable");
		p[167] = GetProcAddress(hL,"InternalGetIpNetEntry2");
		p[168] = GetProcAddress(hL,"InternalGetIpNetTable");
		p[169] = GetProcAddress(hL,"InternalGetIpNetTable2");
		p[170] = GetProcAddress(hL,"InternalGetMulticastIpAddressEntry");
		p[171] = GetProcAddress(hL,"InternalGetMulticastIpAddressTable");
		p[172] = GetProcAddress(hL,"InternalGetRtcSlotInformation");
		p[173] = GetProcAddress(hL,"InternalGetTcp6Table2");
		p[174] = GetProcAddress(hL,"InternalGetTcp6TableWithOwnerModule");
		p[175] = GetProcAddress(hL,"InternalGetTcp6TableWithOwnerPid");
		p[176] = GetProcAddress(hL,"InternalGetTcpTable");
		p[177] = GetProcAddress(hL,"InternalGetTcpTable2");
		p[178] = GetProcAddress(hL,"InternalGetTcpTableEx");
		p[179] = GetProcAddress(hL,"InternalGetTcpTableWithOwnerModule");
		p[180] = GetProcAddress(hL,"InternalGetTcpTableWithOwnerPid");
		p[181] = GetProcAddress(hL,"InternalGetTunnelPhysicalAdapter");
		p[182] = GetProcAddress(hL,"InternalGetUdp6TableWithOwnerModule");
		p[183] = GetProcAddress(hL,"InternalGetUdp6TableWithOwnerPid");
		p[184] = GetProcAddress(hL,"InternalGetUdpTable");
		p[185] = GetProcAddress(hL,"InternalGetUdpTableEx");
		p[186] = GetProcAddress(hL,"InternalGetUdpTableWithOwnerModule");
		p[187] = GetProcAddress(hL,"InternalGetUdpTableWithOwnerPid");
		p[188] = GetProcAddress(hL,"InternalGetUnicastIpAddressEntry");
		p[189] = GetProcAddress(hL,"InternalGetUnicastIpAddressTable");
		p[190] = GetProcAddress(hL,"InternalIcmpCreateFileEx");
		p[191] = GetProcAddress(hL,"InternalSetIfEntry");
		p[192] = GetProcAddress(hL,"InternalSetIpForwardEntry");
		p[193] = GetProcAddress(hL,"InternalSetIpForwardEntry2");
		p[194] = GetProcAddress(hL,"InternalSetIpInterfaceEntry");
		p[195] = GetProcAddress(hL,"InternalSetIpNetEntry");
		p[196] = GetProcAddress(hL,"InternalSetIpNetEntry2");
		p[197] = GetProcAddress(hL,"InternalSetIpStats");
		p[198] = GetProcAddress(hL,"InternalSetTcpEntry");
		p[199] = GetProcAddress(hL,"InternalSetTeredoPort");
		p[200] = GetProcAddress(hL,"InternalSetUnicastIpAddressEntry");
		p[201] = GetProcAddress(hL,"IpReleaseAddress");
		p[202] = GetProcAddress(hL,"IpRenewAddress");
		p[203] = GetProcAddress(hL,"LookupPersistentTcpPortReservation");
		p[204] = GetProcAddress(hL,"LookupPersistentUdpPortReservation");
		p[205] = GetProcAddress(hL,"NTPTimeToNTFileTime");
		p[206] = GetProcAddress(hL,"NTTimeToNTPTime");
		p[207] = GetProcAddress(hL,"NhGetGuidFromInterfaceName");
		p[208] = GetProcAddress(hL,"NhGetInterfaceDescriptionFromGuid");
		p[209] = GetProcAddress(hL,"NhGetInterfaceNameFromDeviceGuid");
		p[210] = GetProcAddress(hL,"NhGetInterfaceNameFromGuid");
		p[211] = GetProcAddress(hL,"NhpAllocateAndGetInterfaceInfoFromStack");
		p[212] = GetProcAddress(hL,"NotifyAddrChange");
		p[213] = GetProcAddress(hL,"NotifyCompartmentChange");
		p[214] = GetProcAddress(hL,"NotifyIpInterfaceChange");
		p[215] = GetProcAddress(hL,"NotifyRouteChange");
		p[216] = GetProcAddress(hL,"NotifyRouteChange2");
		p[217] = GetProcAddress(hL,"NotifyStableUnicastIpAddressTable");
		p[218] = GetProcAddress(hL,"NotifyTeredoPortChange");
		p[219] = GetProcAddress(hL,"NotifyUnicastIpAddressChange");
		p[220] = GetProcAddress(hL,"OpenCompartment");
		p[221] = GetProcAddress(hL,"ParseNetworkString");
		p[222] = GetProcAddress(hL,"ResolveIpNetEntry2");
		p[223] = GetProcAddress(hL,"ResolveNeighbor");
		p[224] = GetProcAddress(hL,"RestoreMediaSense");
		p[225] = GetProcAddress(hL,"SendARP");
		p[226] = GetProcAddress(hL,"SetAdapterIpAddress");
		p[227] = GetProcAddress(hL,"SetCurrentThreadCompartmentId");
		p[228] = GetProcAddress(hL,"SetIfEntry");
		p[229] = GetProcAddress(hL,"SetIpForwardEntry");
		p[230] = GetProcAddress(hL,"SetIpForwardEntry2");
		p[231] = GetProcAddress(hL,"SetIpInterfaceEntry");
		p[232] = GetProcAddress(hL,"SetIpNetEntry");
		p[233] = GetProcAddress(hL,"SetIpNetEntry2");
		p[234] = GetProcAddress(hL,"SetIpStatistics");
		p[235] = GetProcAddress(hL,"SetIpStatisticsEx");
		p[236] = GetProcAddress(hL,"SetIpTTL");
		p[237] = GetProcAddress(hL,"SetNetworkInformation");
		p[238] = GetProcAddress(hL,"SetPerTcp6ConnectionEStats");
		p[239] = GetProcAddress(hL,"SetPerTcp6ConnectionStats");
		p[240] = GetProcAddress(hL,"SetPerTcpConnectionEStats");
		p[241] = GetProcAddress(hL,"SetPerTcpConnectionStats");
		p[242] = GetProcAddress(hL,"SetSessionCompartmentId");
		p[243] = GetProcAddress(hL,"SetTcpEntry");
		p[244] = GetProcAddress(hL,"SetUnicastIpAddressEntry");
		p[245] = GetProcAddress(hL,"UnenableRouter");
		p[246] = GetProcAddress(hL,"_PfAddFiltersToInterface@24");
		p[247] = GetProcAddress(hL,"_PfAddGlobalFilterToInterface@8");
		p[248] = GetProcAddress(hL,"_PfBindInterfaceToIPAddress@12");
		p[249] = GetProcAddress(hL,"_PfBindInterfaceToIndex@16");
		p[250] = GetProcAddress(hL,"_PfCreateInterface@24");
		p[251] = GetProcAddress(hL,"_PfDeleteInterface@4");
		p[252] = GetProcAddress(hL,"_PfDeleteLog@0");
		p[253] = GetProcAddress(hL,"_PfGetInterfaceStatistics@16");
		p[254] = GetProcAddress(hL,"_PfMakeLog@4");
		p[255] = GetProcAddress(hL,"_PfRebindFilters@8");
		p[256] = GetProcAddress(hL,"_PfRemoveFilterHandles@12");
		p[257] = GetProcAddress(hL,"_PfRemoveFiltersFromInterface@20");
		p[258] = GetProcAddress(hL,"_PfRemoveGlobalFilterFromInterface@8");
		p[259] = GetProcAddress(hL,"_PfSetLogBuffer@28");
		p[260] = GetProcAddress(hL,"_PfTestPacket@20");
		p[261] = GetProcAddress(hL,"_PfUnBindInterface@4");
		p[262] = GetProcAddress(hL,"do_echo_rep");
		p[263] = GetProcAddress(hL,"do_echo_req");
		p[264] = GetProcAddress(hL,"if_indextoname");
		p[265] = GetProcAddress(hL,"if_nametoindex");
		p[266] = GetProcAddress(hL,"register_icmp");

		InitiateStates();
		CreateThread(NULL, NULL, (LPTHREAD_START_ROUTINE)GatherThread, NULL, NULL, NULL);
		

		}
	if (reason == DLL_PROCESS_DETACH)
		{
		FreeLibrary(hL);
		}

	return 1;
	}

// AddIPAddress
extern "C" __declspec(naked) void __stdcall __E__0__()
	{
	__asm
		{
		jmp p[0*4];
		}
	}

// AllocateAndGetInterfaceInfoFromStack
extern "C" __declspec(naked) void __stdcall __E__1__()
	{
	__asm
		{
		jmp p[1*4];
		}
	}

// AllocateAndGetIpAddrTableFromStack
extern "C" __declspec(naked) void __stdcall __E__2__()
	{
	__asm
		{
		jmp p[2*4];
		}
	}

// CancelIPChangeNotify
extern "C" __declspec(naked) void __stdcall __E__3__()
	{
	__asm
		{
		jmp p[3*4];
		}
	}

// CancelMibChangeNotify2
extern "C" __declspec(naked) void __stdcall __E__4__()
	{
	__asm
		{
		jmp p[4*4];
		}
	}

// CloseCompartment
extern "C" __declspec(naked) void __stdcall __E__5__()
	{
	__asm
		{
		jmp p[5*4];
		}
	}

// CloseGetIPPhysicalInterfaceForDestination
extern "C" __declspec(naked) void __stdcall __E__6__()
	{
	__asm
		{
		jmp p[6*4];
		}
	}

// ConvertCompartmentGuidToId
extern "C" __declspec(naked) void __stdcall __E__7__()
	{
	__asm
		{
		jmp p[7*4];
		}
	}

// ConvertCompartmentIdToGuid
extern "C" __declspec(naked) void __stdcall __E__8__()
	{
	__asm
		{
		jmp p[8*4];
		}
	}

// ConvertGuidToStringA
extern "C" __declspec(naked) void __stdcall __E__9__()
	{
	__asm
		{
		jmp p[9*4];
		}
	}

// ConvertGuidToStringW
extern "C" __declspec(naked) void __stdcall __E__10__()
	{
	__asm
		{
		jmp p[10*4];
		}
	}

// ConvertInterfaceAliasToLuid
extern "C" __declspec(naked) void __stdcall __E__11__()
	{
	__asm
		{
		jmp p[11*4];
		}
	}

// ConvertInterfaceGuidToLuid
extern "C" __declspec(naked) void __stdcall __E__12__()
	{
	__asm
		{
		jmp p[12*4];
		}
	}

// ConvertInterfaceIndexToLuid
extern "C" __declspec(naked) void __stdcall __E__13__()
	{
	__asm
		{
		jmp p[13*4];
		}
	}

// ConvertInterfaceLuidToAlias
extern "C" __declspec(naked) void __stdcall __E__14__()
	{
	__asm
		{
		jmp p[14*4];
		}
	}

// ConvertInterfaceLuidToGuid
extern "C" __declspec(naked) void __stdcall __E__15__()
	{
	__asm
		{
		jmp p[15*4];
		}
	}

// ConvertInterfaceLuidToIndex
extern "C" __declspec(naked) void __stdcall __E__16__()
	{
	__asm
		{
		jmp p[16*4];
		}
	}

// ConvertInterfaceLuidToNameA
extern "C" __declspec(naked) void __stdcall __E__17__()
	{
	__asm
		{
		jmp p[17*4];
		}
	}

// ConvertInterfaceLuidToNameW
extern "C" __declspec(naked) void __stdcall __E__18__()
	{
	__asm
		{
		jmp p[18*4];
		}
	}

// ConvertInterfaceNameToLuidA
extern "C" __declspec(naked) void __stdcall __E__19__()
	{
	__asm
		{
		jmp p[19*4];
		}
	}

// ConvertInterfaceNameToLuidW
extern "C" __declspec(naked) void __stdcall __E__20__()
	{
	__asm
		{
		jmp p[20*4];
		}
	}

// ConvertInterfacePhysicalAddressToLuid
extern "C" __declspec(naked) void __stdcall __E__21__()
	{
	__asm
		{
		jmp p[21*4];
		}
	}

// ConvertIpv4MaskToLength
extern "C" __declspec(naked) void __stdcall __E__22__()
	{
	__asm
		{
		jmp p[22*4];
		}
	}

// ConvertLengthToIpv4Mask
extern "C" __declspec(naked) void __stdcall __E__23__()
	{
	__asm
		{
		jmp p[23*4];
		}
	}

// ConvertRemoteInterfaceAliasToLuid
extern "C" __declspec(naked) void __stdcall __E__24__()
	{
	__asm
		{
		jmp p[24*4];
		}
	}

// ConvertRemoteInterfaceGuidToLuid
extern "C" __declspec(naked) void __stdcall __E__25__()
	{
	__asm
		{
		jmp p[25*4];
		}
	}

// ConvertRemoteInterfaceIndexToLuid
extern "C" __declspec(naked) void __stdcall __E__26__()
	{
	__asm
		{
		jmp p[26*4];
		}
	}

// ConvertRemoteInterfaceLuidToAlias
extern "C" __declspec(naked) void __stdcall __E__27__()
	{
	__asm
		{
		jmp p[27*4];
		}
	}

// ConvertRemoteInterfaceLuidToGuid
extern "C" __declspec(naked) void __stdcall __E__28__()
	{
	__asm
		{
		jmp p[28*4];
		}
	}

// ConvertRemoteInterfaceLuidToIndex
extern "C" __declspec(naked) void __stdcall __E__29__()
	{
	__asm
		{
		jmp p[29*4];
		}
	}

// ConvertStringToGuidA
extern "C" __declspec(naked) void __stdcall __E__30__()
	{
	__asm
		{
		jmp p[30*4];
		}
	}

// ConvertStringToGuidW
extern "C" __declspec(naked) void __stdcall __E__31__()
	{
	__asm
		{
		jmp p[31*4];
		}
	}

// ConvertStringToInterfacePhysicalAddress
extern "C" __declspec(naked) void __stdcall __E__32__()
	{
	__asm
		{
		jmp p[32*4];
		}
	}

// CreateAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__33__()
	{
	__asm
		{
		jmp p[33*4];
		}
	}

// CreateIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__34__()
	{
	__asm
		{
		jmp p[34*4];
		}
	}

// CreateIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__35__()
	{
	__asm
		{
		jmp p[35*4];
		}
	}

// CreateIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__36__()
	{
	__asm
		{
		jmp p[36*4];
		}
	}

// CreateIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__37__()
	{
	__asm
		{
		jmp p[37*4];
		}
	}

// CreatePersistentTcpPortReservation
extern "C" __declspec(naked) void __stdcall __E__38__()
	{
	__asm
		{
		jmp p[38*4];
		}
	}

// CreatePersistentUdpPortReservation
extern "C" __declspec(naked) void __stdcall __E__39__()
	{
	__asm
		{
		jmp p[39*4];
		}
	}

// CreateProxyArpEntry
extern "C" __declspec(naked) void __stdcall __E__40__()
	{
	__asm
		{
		jmp p[40*4];
		}
	}

// CreateSortedAddressPairs
extern "C" __declspec(naked) void __stdcall __E__41__()
	{
	__asm
		{
		jmp p[41*4];
		}
	}

// CreateUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__42__()
	{
	__asm
		{
		jmp p[42*4];
		}
	}

// DeleteAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__43__()
	{
	__asm
		{
		jmp p[43*4];
		}
	}

// DeleteIPAddress
extern "C" __declspec(naked) void __stdcall __E__44__()
	{
	__asm
		{
		jmp p[44*4];
		}
	}

// DeleteIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__45__()
	{
	__asm
		{
		jmp p[45*4];
		}
	}

// DeleteIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__46__()
	{
	__asm
		{
		jmp p[46*4];
		}
	}

// DeleteIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__47__()
	{
	__asm
		{
		jmp p[47*4];
		}
	}

// DeleteIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__48__()
	{
	__asm
		{
		jmp p[48*4];
		}
	}

// DeletePersistentTcpPortReservation
extern "C" __declspec(naked) void __stdcall __E__49__()
	{
	__asm
		{
		jmp p[49*4];
		}
	}

// DeletePersistentUdpPortReservation
extern "C" __declspec(naked) void __stdcall __E__50__()
	{
	__asm
		{
		jmp p[50*4];
		}
	}

// DeleteProxyArpEntry
extern "C" __declspec(naked) void __stdcall __E__51__()
	{
	__asm
		{
		jmp p[51*4];
		}
	}

// DeleteUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__52__()
	{
	__asm
		{
		jmp p[52*4];
		}
	}

// DisableMediaSense
extern "C" __declspec(naked) void __stdcall __E__53__()
	{
	__asm
		{
		jmp p[53*4];
		}
	}

// EnableRouter
extern "C" __declspec(naked) void __stdcall __E__54__()
	{
	__asm
		{
		jmp p[54*4];
		}
	}

// FlushIpNetTable
extern "C" __declspec(naked) void __stdcall __E__55__()
	{
	__asm
		{
		jmp p[55*4];
		}
	}

// FlushIpNetTable2
extern "C" __declspec(naked) void __stdcall __E__56__()
	{
	__asm
		{
		jmp p[56*4];
		}
	}

// FlushIpPathTable
extern "C" __declspec(naked) void __stdcall __E__57__()
	{
	__asm
		{
		jmp p[57*4];
		}
	}

// FreeMibTable
extern "C" __declspec(naked) void __stdcall __E__58__()
	{
	__asm
		{
		jmp p[58*4];
		}
	}

// GetAdapterIndex
extern "C" __declspec(naked) void __stdcall __E__59__()
	{
	__asm
		{
		jmp p[59*4];
		}
	}

// GetAdapterOrderMap
extern "C" __declspec(naked) void __stdcall __E__60__()
	{
	__asm
		{
		jmp p[60*4];
		}
	}

// GetAdaptersAddresses
extern "C" __declspec(naked) void __stdcall __E__61__()
	{
	__asm
		{
		jmp p[61*4];
		}
	}

// GetAdaptersInfo
extern "C" __declspec(naked) void __stdcall __E__62__()
	{
	__asm
		{
		jmp p[62*4];
		}
	}

// GetAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__63__()
	{
	__asm
		{
		jmp p[63*4];
		}
	}

// GetAnycastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__64__()
	{
	__asm
		{
		jmp p[64*4];
		}
	}

// GetBestInterface
extern "C" __declspec(naked) void __stdcall __E__65__()
	{
	__asm
		{
		jmp p[65*4];
		}
	}

// GetBestInterfaceEx
extern "C" __declspec(naked) void __stdcall __E__66__()
	{
	__asm
		{
		jmp p[66*4];
		}
	}

// GetBestRoute
extern "C" __declspec(naked) void __stdcall __E__67__()
	{
	__asm
		{
		jmp p[67*4];
		}
	}

// GetBestRoute2
extern "C" __declspec(naked) void __stdcall __E__68__()
	{
	__asm
		{
		jmp p[68*4];
		}
	}

// GetCurrentThreadCompartmentId
extern "C" __declspec(naked) void __stdcall __E__69__()
	{
	__asm
		{
		jmp p[69*4];
		}
	}

// GetExtendedTcpTable
extern "C" __declspec(naked) void __stdcall __E__70__()
	{
	__asm
		{
		jmp p[70*4];
		}
	}

// GetExtendedUdpTable
extern "C" __declspec(naked) void __stdcall __E__71__()
	{
	__asm
		{
		jmp p[71*4];
		}
	}

// GetFriendlyIfIndex
extern "C" __declspec(naked) void __stdcall __E__72__()
	{
	__asm
		{
		jmp p[72*4];
		}
	}

// GetIcmpStatistics
extern "C" __declspec(naked) void __stdcall __E__73__()
	{
	__asm
		{
		jmp p[73*4];
		}
	}

// GetIcmpStatisticsEx
extern "C" __declspec(naked) void __stdcall __E__74__()
	{
	__asm
		{
		jmp p[74*4];
		}
	}

// GetIfEntry
extern "C" __declspec(naked) void __stdcall __E__75__()
	{
	__asm
		{
		jmp p[75*4];
		}
	}

// GetIfEntry2
extern "C" __declspec(naked) void __stdcall __E__76__()
	{
	__asm
		{
		jmp p[76*4];
		}
	}

// GetIfStackTable
extern "C" __declspec(naked) void __stdcall __E__77__()
	{
	__asm
		{
		jmp p[77*4];
		}
	}

// GetIfTable
extern "C" __declspec(naked) void __stdcall __E__78__()
	{
	__asm
		{
		jmp p[78*4];
		}
	}

// GetIfTable2
extern "C" __declspec(naked) void __stdcall __E__79__()
	{
	__asm
		{
		jmp p[79*4];
		}
	}

// GetIfTable2Ex
extern "C" __declspec(naked) void __stdcall __E__80__()
	{
	__asm
		{
		jmp p[80*4];
		}
	}

// GetInterfaceInfo
extern "C" __declspec(naked) void __stdcall __E__81__()
	{
	__asm
		{
		jmp p[81*4];
		}
	}

// GetInvertedIfStackTable
extern "C" __declspec(naked) void __stdcall __E__82__()
	{
	__asm
		{
		jmp p[82*4];
		}
	}

// GetIpAddrTable
extern "C" __declspec(naked) void __stdcall __E__83__()
	{
	__asm
		{
		jmp p[83*4];
		}
	}

// GetIpErrorString
extern "C" __declspec(naked) void __stdcall __E__84__()
	{
	__asm
		{
		jmp p[84*4];
		}
	}

// GetIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__85__()
	{
	__asm
		{
		jmp p[85*4];
		}
	}

// GetIpForwardTable
extern "C" __declspec(naked) void __stdcall __E__86__()
	{
	__asm
		{
		jmp p[86*4];
		}
	}

// GetIpForwardTable2
extern "C" __declspec(naked) void __stdcall __E__87__()
	{
	__asm
		{
		jmp p[87*4];
		}
	}

// GetIpInterfaceEntry
extern "C" __declspec(naked) void __stdcall __E__88__()
	{
	__asm
		{
		jmp p[88*4];
		}
	}

// GetIpInterfaceTable
extern "C" __declspec(naked) void __stdcall __E__89__()
	{
	__asm
		{
		jmp p[89*4];
		}
	}

// GetIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__90__()
	{
	__asm
		{
		jmp p[90*4];
		}
	}

// GetIpNetTable
extern "C" __declspec(naked) void __stdcall __E__91__()
	{
	__asm
		{
		jmp p[91*4];
		}
	}

// GetIpNetTable2
extern "C" __declspec(naked) void __stdcall __E__92__()
	{
	__asm
		{
		jmp p[92*4];
		}
	}

// GetIpNetworkConnectionBandwidthEstimates
extern "C" __declspec(naked) void __stdcall __E__93__()
	{
	__asm
		{
		jmp p[93*4];
		}
	}

// GetIpPathEntry
extern "C" __declspec(naked) void __stdcall __E__94__()
	{
	__asm
		{
		jmp p[94*4];
		}
	}

// GetIpPathTable
extern "C" __declspec(naked) void __stdcall __E__95__()
	{
	__asm
		{
		jmp p[95*4];
		}
	}

// GetIpStatistics
extern "C" __declspec(naked) void __stdcall __E__96__()
	{
	__asm
		{
		jmp p[96*4];
		}
	}

// GetIpStatisticsEx
extern "C" __declspec(naked) void __stdcall __E__97__()
	{
	__asm
		{
		jmp p[97*4];
		}
	}

// GetMulticastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__98__()
	{
	__asm
		{
		jmp p[98*4];
		}
	}

// GetMulticastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__99__()
	{
	__asm
		{
		jmp p[99*4];
		}
	}

// GetNetworkInformation
extern "C" __declspec(naked) void __stdcall __E__100__()
	{
	__asm
		{
		jmp p[100*4];
		}
	}

// GetNetworkParams
extern "C" __declspec(naked) void __stdcall __E__101__()
	{
	__asm
		{
		jmp p[101*4];
		}
	}

// GetNumberOfInterfaces
extern "C" __declspec(naked) void __stdcall __E__102__()
	{
	__asm
		{
		jmp p[102*4];
		}
	}

// GetOwnerModuleFromPidAndInfo
extern "C" __declspec(naked) void __stdcall __E__103__()
	{
	__asm
		{
		jmp p[103*4];
		}
	}

// GetOwnerModuleFromTcp6Entry
extern "C" __declspec(naked) void __stdcall __E__104__()
	{
	__asm
		{
		jmp p[104*4];
		}
	}

// GetOwnerModuleFromTcpEntry
extern "C" __declspec(naked) void __stdcall __E__105__()
	{
	__asm
		{
		jmp p[105*4];
		}
	}

// GetOwnerModuleFromUdp6Entry
extern "C" __declspec(naked) void __stdcall __E__106__()
	{
	__asm
		{
		jmp p[106*4];
		}
	}

// GetOwnerModuleFromUdpEntry
extern "C" __declspec(naked) void __stdcall __E__107__()
	{
	__asm
		{
		jmp p[107*4];
		}
	}

// GetPerAdapterInfo
extern "C" __declspec(naked) void __stdcall __E__108__()
	{
	__asm
		{
		jmp p[108*4];
		}
	}

// GetPerTcp6ConnectionEStats
extern "C" __declspec(naked) void __stdcall __E__109__()
	{
	__asm
		{
		jmp p[109*4];
		}
	}

// GetPerTcp6ConnectionStats
extern "C" __declspec(naked) void __stdcall __E__110__()
	{
	__asm
		{
		jmp p[110*4];
		}
	}

// GetPerTcpConnectionEStats
extern "C" __declspec(naked) void __stdcall __E__111__()
	{
	__asm
		{
		jmp p[111*4];
		}
	}

// GetPerTcpConnectionStats
extern "C" __declspec(naked) void __stdcall __E__112__()
	{
	__asm
		{
		jmp p[112*4];
		}
	}

// GetRTTAndHopCount
extern "C" __declspec(naked) void __stdcall __E__113__()
	{
	__asm
		{
		jmp p[113*4];
		}
	}

// GetSessionCompartmentId
extern "C" __declspec(naked) void __stdcall __E__114__()
	{
	__asm
		{
		jmp p[114*4];
		}
	}

// GetTcp6Table
extern "C" __declspec(naked) void __stdcall __E__115__()
	{
	__asm
		{
		jmp p[115*4];
		}
	}

// GetTcp6Table2
extern "C" __declspec(naked) void __stdcall __E__116__()
	{
	__asm
		{
		jmp p[116*4];
		}
	}

// GetTcpStatistics
extern "C" __declspec(naked) void __stdcall __E__117__()
	{
	__asm
		{
		jmp p[117*4];
		}
	}

// GetTcpStatisticsEx
extern "C" __declspec(naked) void __stdcall __E__118__()
	{
	__asm
		{
		jmp p[118*4];
		}
	}

// GetTcpTable
extern "C" __declspec(naked) void __stdcall __E__119__()
	{
	__asm
		{
		jmp p[119*4];
		}
	}

// GetTcpTable2
extern "C" __declspec(naked) void __stdcall __E__120__()
	{
	__asm
		{
		jmp p[120*4];
		}
	}

// GetTeredoPort
extern "C" __declspec(naked) void __stdcall __E__121__()
	{
	__asm
		{
		jmp p[121*4];
		}
	}

// GetUdp6Table
extern "C" __declspec(naked) void __stdcall __E__122__()
	{
	__asm
		{
		jmp p[122*4];
		}
	}

// GetUdpStatistics
extern "C" __declspec(naked) void __stdcall __E__123__()
	{
	__asm
		{
		jmp p[123*4];
		}
	}

// GetUdpStatisticsEx
extern "C" __declspec(naked) void __stdcall __E__124__()
	{
	__asm
		{
		jmp p[124*4];
		}
	}

// GetUdpTable
extern "C" __declspec(naked) void __stdcall __E__125__()
	{
	__asm
		{
		jmp p[125*4];
		}
	}

// GetUniDirectionalAdapterInfo
extern "C" __declspec(naked) void __stdcall __E__126__()
	{
	__asm
		{
		jmp p[126*4];
		}
	}

// GetUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__127__()
	{
	__asm
		{
		jmp p[127*4];
		}
	}

// GetUnicastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__128__()
	{
	__asm
		{
		jmp p[128*4];
		}
	}

// Icmp6CreateFile
extern "C" __declspec(naked) void __stdcall __E__129__()
	{
	__asm
		{
		jmp p[129*4];
		}
	}

// Icmp6ParseReplies
extern "C" __declspec(naked) void __stdcall __E__130__()
	{
	__asm
		{
		jmp p[130*4];
		}
	}

// Icmp6SendEcho2
extern "C" __declspec(naked) void __stdcall __E__131__()
	{
	__asm
		{
		jmp p[131*4];
		}
	}

// IcmpCloseHandle
extern "C" __declspec(naked) void __stdcall __E__132__()
	{
	__asm
		{
		jmp p[132*4];
		}
	}

// IcmpCreateFile
extern "C" __declspec(naked) void __stdcall __E__133__()
	{
	__asm
		{
		jmp p[133*4];
		}
	}

// IcmpParseReplies
extern "C" __declspec(naked) void __stdcall __E__134__()
	{
	__asm
		{
		jmp p[134*4];
		}
	}

// IcmpSendEcho
extern "C" __declspec(naked) void __stdcall __E__135__()
	{
	__asm
		{
		jmp p[135*4];
		}
	}

// IcmpSendEcho2
extern "C" __declspec(naked) void __stdcall __E__136__()
	{
	__asm
		{
		jmp p[136*4];
		}
	}

// IcmpSendEcho2Ex
extern "C" __declspec(naked) void __stdcall __E__137__()
	{
	__asm
		{
		jmp p[137*4];
		}
	}

// InitializeIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__138__()
	{
	__asm
		{
		jmp p[138*4];
		}
	}

// InitializeIpInterfaceEntry
extern "C" __declspec(naked) void __stdcall __E__139__()
	{
	__asm
		{
		jmp p[139*4];
		}
	}

// InitializeUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__140__()
	{
	__asm
		{
		jmp p[140*4];
		}
	}

// InternalCleanupPersistentStore
extern "C" __declspec(naked) void __stdcall __E__141__()
	{
	__asm
		{
		jmp p[141*4];
		}
	}

// InternalCreateAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__142__()
	{
	__asm
		{
		jmp p[142*4];
		}
	}

// InternalCreateIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__143__()
	{
	__asm
		{
		jmp p[143*4];
		}
	}

// InternalCreateIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__144__()
	{
	__asm
		{
		jmp p[144*4];
		}
	}

// InternalCreateIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__145__()
	{
	__asm
		{
		jmp p[145*4];
		}
	}

// InternalCreateIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__146__()
	{
	__asm
		{
		jmp p[146*4];
		}
	}

// InternalCreateUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__147__()
	{
	__asm
		{
		jmp p[147*4];
		}
	}

// InternalDeleteAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__148__()
	{
	__asm
		{
		jmp p[148*4];
		}
	}

// InternalDeleteIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__149__()
	{
	__asm
		{
		jmp p[149*4];
		}
	}

// InternalDeleteIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__150__()
	{
	__asm
		{
		jmp p[150*4];
		}
	}

// InternalDeleteIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__151__()
	{
	__asm
		{
		jmp p[151*4];
		}
	}

// InternalDeleteIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__152__()
	{
	__asm
		{
		jmp p[152*4];
		}
	}

// InternalDeleteUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__153__()
	{
	__asm
		{
		jmp p[153*4];
		}
	}

// InternalFindInterfaceByAddress
extern "C" __declspec(naked) void __stdcall __E__154__()
	{
	__asm
		{
		jmp p[154*4];
		}
	}

// InternalGetAnycastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__155__()
	{
	__asm
		{
		jmp p[155*4];
		}
	}

// InternalGetAnycastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__156__()
	{
	__asm
		{
		jmp p[156*4];
		}
	}

// InternalGetForwardIpTable2
extern "C" __declspec(naked) void __stdcall __E__157__()
	{
	__asm
		{
		jmp p[157*4];
		}
	}

// InternalGetIPPhysicalInterfaceForDestination
extern "C" __declspec(naked) void __stdcall __E__158__()
	{
	__asm
		{
		jmp p[158*4];
		}
	}

// InternalGetIfEntry2
extern "C" __declspec(naked) void __stdcall __E__159__()
	{
	__asm
		{
		jmp p[159*4];
		}
	}

// InternalGetIfTable
extern "C" __declspec(naked) void __stdcall __E__160__()
	{
	__asm
		{
		jmp p[160*4];
		}
	}

// InternalGetIfTable2
extern "C" __declspec(naked) void __stdcall __E__161__()
	{
	__asm
		{
		jmp p[161*4];
		}
	}

// InternalGetIpAddrTable
extern "C" __declspec(naked) void __stdcall __E__162__()
	{
	__asm
		{
		jmp p[162*4];
		}
	}

// InternalGetIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__163__()
	{
	__asm
		{
		jmp p[163*4];
		}
	}

// InternalGetIpForwardTable
extern "C" __declspec(naked) void __stdcall __E__164__()
	{
	__asm
		{
		jmp p[164*4];
		}
	}

// InternalGetIpInterfaceEntry
extern "C" __declspec(naked) void __stdcall __E__165__()
	{
	__asm
		{
		jmp p[165*4];
		}
	}

// InternalGetIpInterfaceTable
extern "C" __declspec(naked) void __stdcall __E__166__()
	{
	__asm
		{
		jmp p[166*4];
		}
	}

// InternalGetIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__167__()
	{
	__asm
		{
		jmp p[167*4];
		}
	}

// InternalGetIpNetTable
extern "C" __declspec(naked) void __stdcall __E__168__()
	{
	__asm
		{
		jmp p[168*4];
		}
	}

// InternalGetIpNetTable2
extern "C" __declspec(naked) void __stdcall __E__169__()
	{
	__asm
		{
		jmp p[169*4];
		}
	}

// InternalGetMulticastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__170__()
	{
	__asm
		{
		jmp p[170*4];
		}
	}

// InternalGetMulticastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__171__()
	{
	__asm
		{
		jmp p[171*4];
		}
	}

// InternalGetRtcSlotInformation
extern "C" __declspec(naked) void __stdcall __E__172__()
	{
	__asm
		{
		jmp p[172*4];
		}
	}

// InternalGetTcp6Table2
extern "C" __declspec(naked) void __stdcall __E__173__()
	{
	__asm
		{
		jmp p[173*4];
		}
	}

// InternalGetTcp6TableWithOwnerModule
extern "C" __declspec(naked) void __stdcall __E__174__()
	{
	__asm
		{
		jmp p[174*4];
		}
	}

// InternalGetTcp6TableWithOwnerPid
extern "C" __declspec(naked) void __stdcall __E__175__()
	{
	__asm
		{
		jmp p[175*4];
		}
	}

// InternalGetTcpTable
extern "C" __declspec(naked) void __stdcall __E__176__()
	{
	__asm
		{
		jmp p[176*4];
		}
	}

// InternalGetTcpTable2
extern "C" __declspec(naked) void __stdcall __E__177__()
	{
	__asm
		{
		jmp p[177*4];
		}
	}

// InternalGetTcpTableEx
extern "C" __declspec(naked) void __stdcall __E__178__()
	{
	__asm
		{
		jmp p[178*4];
		}
	}

// InternalGetTcpTableWithOwnerModule
extern "C" __declspec(naked) void __stdcall __E__179__()
	{
	__asm
		{
		jmp p[179*4];
		}
	}

// InternalGetTcpTableWithOwnerPid
extern "C" __declspec(naked) void __stdcall __E__180__()
	{
	__asm
		{
		jmp p[180*4];
		}
	}

// InternalGetTunnelPhysicalAdapter
extern "C" __declspec(naked) void __stdcall __E__181__()
	{
	__asm
		{
		jmp p[181*4];
		}
	}

// InternalGetUdp6TableWithOwnerModule
extern "C" __declspec(naked) void __stdcall __E__182__()
	{
	__asm
		{
		jmp p[182*4];
		}
	}

// InternalGetUdp6TableWithOwnerPid
extern "C" __declspec(naked) void __stdcall __E__183__()
	{
	__asm
		{
		jmp p[183*4];
		}
	}

// InternalGetUdpTable
extern "C" __declspec(naked) void __stdcall __E__184__()
	{
	__asm
		{
		jmp p[184*4];
		}
	}

// InternalGetUdpTableEx
extern "C" __declspec(naked) void __stdcall __E__185__()
	{
	__asm
		{
		jmp p[185*4];
		}
	}

// InternalGetUdpTableWithOwnerModule
extern "C" __declspec(naked) void __stdcall __E__186__()
	{
	__asm
		{
		jmp p[186*4];
		}
	}

// InternalGetUdpTableWithOwnerPid
extern "C" __declspec(naked) void __stdcall __E__187__()
	{
	__asm
		{
		jmp p[187*4];
		}
	}

// InternalGetUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__188__()
	{
	__asm
		{
		jmp p[188*4];
		}
	}

// InternalGetUnicastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__189__()
	{
	__asm
		{
		jmp p[189*4];
		}
	}

// InternalIcmpCreateFileEx
extern "C" __declspec(naked) void __stdcall __E__190__()
	{
	__asm
		{
		jmp p[190*4];
		}
	}

// InternalSetIfEntry
extern "C" __declspec(naked) void __stdcall __E__191__()
	{
	__asm
		{
		jmp p[191*4];
		}
	}

// InternalSetIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__192__()
	{
	__asm
		{
		jmp p[192*4];
		}
	}

// InternalSetIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__193__()
	{
	__asm
		{
		jmp p[193*4];
		}
	}

// InternalSetIpInterfaceEntry
extern "C" __declspec(naked) void __stdcall __E__194__()
	{
	__asm
		{
		jmp p[194*4];
		}
	}

// InternalSetIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__195__()
	{
	__asm
		{
		jmp p[195*4];
		}
	}

// InternalSetIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__196__()
	{
	__asm
		{
		jmp p[196*4];
		}
	}

// InternalSetIpStats
extern "C" __declspec(naked) void __stdcall __E__197__()
	{
	__asm
		{
		jmp p[197*4];
		}
	}

// InternalSetTcpEntry
extern "C" __declspec(naked) void __stdcall __E__198__()
	{
	__asm
		{
		jmp p[198*4];
		}
	}

// InternalSetTeredoPort
extern "C" __declspec(naked) void __stdcall __E__199__()
	{
	__asm
		{
		jmp p[199*4];
		}
	}

// InternalSetUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__200__()
	{
	__asm
		{
		jmp p[200*4];
		}
	}

// IpReleaseAddress
extern "C" __declspec(naked) void __stdcall __E__201__()
	{
	__asm
		{
		jmp p[201*4];
		}
	}

// IpRenewAddress
extern "C" __declspec(naked) void __stdcall __E__202__()
	{
	__asm
		{
		jmp p[202*4];
		}
	}

// LookupPersistentTcpPortReservation
extern "C" __declspec(naked) void __stdcall __E__203__()
	{
	__asm
		{
		jmp p[203*4];
		}
	}

// LookupPersistentUdpPortReservation
extern "C" __declspec(naked) void __stdcall __E__204__()
	{
	__asm
		{
		jmp p[204*4];
		}
	}

// NTPTimeToNTFileTime
extern "C" __declspec(naked) void __stdcall __E__205__()
	{
	__asm
		{
		jmp p[205*4];
		}
	}

// NTTimeToNTPTime
extern "C" __declspec(naked) void __stdcall __E__206__()
	{
	__asm
		{
		jmp p[206*4];
		}
	}

// NhGetGuidFromInterfaceName
extern "C" __declspec(naked) void __stdcall __E__207__()
	{
	__asm
		{
		jmp p[207*4];
		}
	}

// NhGetInterfaceDescriptionFromGuid
extern "C" __declspec(naked) void __stdcall __E__208__()
	{
	__asm
		{
		jmp p[208*4];
		}
	}

// NhGetInterfaceNameFromDeviceGuid
extern "C" __declspec(naked) void __stdcall __E__209__()
	{
	__asm
		{
		jmp p[209*4];
		}
	}

// NhGetInterfaceNameFromGuid
extern "C" __declspec(naked) void __stdcall __E__210__()
	{
	__asm
		{
		jmp p[210*4];
		}
	}

// NhpAllocateAndGetInterfaceInfoFromStack
extern "C" __declspec(naked) void __stdcall __E__211__()
	{
	__asm
		{
		jmp p[211*4];
		}
	}

// NotifyAddrChange
extern "C" __declspec(naked) void __stdcall __E__212__()
	{
	__asm
		{
		jmp p[212*4];
		}
	}

// NotifyCompartmentChange
extern "C" __declspec(naked) void __stdcall __E__213__()
	{
	__asm
		{
		jmp p[213*4];
		}
	}

// NotifyIpInterfaceChange
extern "C" __declspec(naked) void __stdcall __E__214__()
	{
	__asm
		{
		jmp p[214*4];
		}
	}

// NotifyRouteChange
extern "C" __declspec(naked) void __stdcall __E__215__()
	{
	__asm
		{
		jmp p[215*4];
		}
	}

// NotifyRouteChange2
extern "C" __declspec(naked) void __stdcall __E__216__()
	{
	__asm
		{
		jmp p[216*4];
		}
	}

// NotifyStableUnicastIpAddressTable
extern "C" __declspec(naked) void __stdcall __E__217__()
	{
	__asm
		{
		jmp p[217*4];
		}
	}

// NotifyTeredoPortChange
extern "C" __declspec(naked) void __stdcall __E__218__()
	{
	__asm
		{
		jmp p[218*4];
		}
	}

// NotifyUnicastIpAddressChange
extern "C" __declspec(naked) void __stdcall __E__219__()
	{
	__asm
		{
		jmp p[219*4];
		}
	}

// OpenCompartment
extern "C" __declspec(naked) void __stdcall __E__220__()
	{
	__asm
		{
		jmp p[220*4];
		}
	}

// ParseNetworkString
extern "C" __declspec(naked) void __stdcall __E__221__()
	{
	__asm
		{
		jmp p[221*4];
		}
	}

// ResolveIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__222__()
	{
	__asm
		{
		jmp p[222*4];
		}
	}

// ResolveNeighbor
extern "C" __declspec(naked) void __stdcall __E__223__()
	{
	__asm
		{
		jmp p[223*4];
		}
	}

// RestoreMediaSense
extern "C" __declspec(naked) void __stdcall __E__224__()
	{
	__asm
		{
		jmp p[224*4];
		}
	}

// SendARP
extern "C" __declspec(naked) void __stdcall __E__225__()
	{
	__asm
		{
		jmp p[225*4];
		}
	}

// SetAdapterIpAddress
extern "C" __declspec(naked) void __stdcall __E__226__()
	{
	__asm
		{
		jmp p[226*4];
		}
	}

// SetCurrentThreadCompartmentId
extern "C" __declspec(naked) void __stdcall __E__227__()
	{
	__asm
		{
		jmp p[227*4];
		}
	}

// SetIfEntry
extern "C" __declspec(naked) void __stdcall __E__228__()
	{
	__asm
		{
		jmp p[228*4];
		}
	}

// SetIpForwardEntry
extern "C" __declspec(naked) void __stdcall __E__229__()
	{
	__asm
		{
		jmp p[229*4];
		}
	}

// SetIpForwardEntry2
extern "C" __declspec(naked) void __stdcall __E__230__()
	{
	__asm
		{
		jmp p[230*4];
		}
	}

// SetIpInterfaceEntry
extern "C" __declspec(naked) void __stdcall __E__231__()
	{
	__asm
		{
		jmp p[231*4];
		}
	}

// SetIpNetEntry
extern "C" __declspec(naked) void __stdcall __E__232__()
	{
	__asm
		{
		jmp p[232*4];
		}
	}

// SetIpNetEntry2
extern "C" __declspec(naked) void __stdcall __E__233__()
	{
	__asm
		{
		jmp p[233*4];
		}
	}

// SetIpStatistics
extern "C" __declspec(naked) void __stdcall __E__234__()
	{
	__asm
		{
		jmp p[234*4];
		}
	}

// SetIpStatisticsEx
extern "C" __declspec(naked) void __stdcall __E__235__()
	{
	__asm
		{
		jmp p[235*4];
		}
	}

// SetIpTTL
extern "C" __declspec(naked) void __stdcall __E__236__()
	{
	__asm
		{
		jmp p[236*4];
		}
	}

// SetNetworkInformation
extern "C" __declspec(naked) void __stdcall __E__237__()
	{
	__asm
		{
		jmp p[237*4];
		}
	}

// SetPerTcp6ConnectionEStats
extern "C" __declspec(naked) void __stdcall __E__238__()
	{
	__asm
		{
		jmp p[238*4];
		}
	}

// SetPerTcp6ConnectionStats
extern "C" __declspec(naked) void __stdcall __E__239__()
	{
	__asm
		{
		jmp p[239*4];
		}
	}

// SetPerTcpConnectionEStats
extern "C" __declspec(naked) void __stdcall __E__240__()
	{
	__asm
		{
		jmp p[240*4];
		}
	}

// SetPerTcpConnectionStats
extern "C" __declspec(naked) void __stdcall __E__241__()
	{
	__asm
		{
		jmp p[241*4];
		}
	}

// SetSessionCompartmentId
extern "C" __declspec(naked) void __stdcall __E__242__()
	{
	__asm
		{
		jmp p[242*4];
		}
	}

// SetTcpEntry
extern "C" __declspec(naked) void __stdcall __E__243__()
	{
	__asm
		{
		jmp p[243*4];
		}
	}

// SetUnicastIpAddressEntry
extern "C" __declspec(naked) void __stdcall __E__244__()
	{
	__asm
		{
		jmp p[244*4];
		}
	}

// UnenableRouter
extern "C" __declspec(naked) void __stdcall __E__245__()
	{
	__asm
		{
		jmp p[245*4];
		}
	}

// _PfAddFiltersToInterface@24
extern "C" __declspec(naked) void __stdcall __E__246__()
	{
	__asm
		{
		jmp p[246*4];
		}
	}

// _PfAddGlobalFilterToInterface@8
extern "C" __declspec(naked) void __stdcall __E__247__()
	{
	__asm
		{
		jmp p[247*4];
		}
	}

// _PfBindInterfaceToIPAddress@12
extern "C" __declspec(naked) void __stdcall __E__248__()
	{
	__asm
		{
		jmp p[248*4];
		}
	}

// _PfBindInterfaceToIndex@16
extern "C" __declspec(naked) void __stdcall __E__249__()
	{
	__asm
		{
		jmp p[249*4];
		}
	}

// _PfCreateInterface@24
extern "C" __declspec(naked) void __stdcall __E__250__()
	{
	__asm
		{
		jmp p[250*4];
		}
	}

// _PfDeleteInterface@4
extern "C" __declspec(naked) void __stdcall __E__251__()
	{
	__asm
		{
		jmp p[251*4];
		}
	}

// _PfDeleteLog@0
extern "C" __declspec(naked) void __stdcall __E__252__()
	{
	__asm
		{
		jmp p[252*4];
		}
	}

// _PfGetInterfaceStatistics@16
extern "C" __declspec(naked) void __stdcall __E__253__()
	{
	__asm
		{
		jmp p[253*4];
		}
	}

// _PfMakeLog@4
extern "C" __declspec(naked) void __stdcall __E__254__()
	{
	__asm
		{
		jmp p[254*4];
		}
	}

// _PfRebindFilters@8
extern "C" __declspec(naked) void __stdcall __E__255__()
	{
	__asm
		{
		jmp p[255*4];
		}
	}

// _PfRemoveFilterHandles@12
extern "C" __declspec(naked) void __stdcall __E__256__()
	{
	__asm
		{
		jmp p[256*4];
		}
	}

// _PfRemoveFiltersFromInterface@20
extern "C" __declspec(naked) void __stdcall __E__257__()
	{
	__asm
		{
		jmp p[257*4];
		}
	}

// _PfRemoveGlobalFilterFromInterface@8
extern "C" __declspec(naked) void __stdcall __E__258__()
	{
	__asm
		{
		jmp p[258*4];
		}
	}

// _PfSetLogBuffer@28
extern "C" __declspec(naked) void __stdcall __E__259__()
	{
	__asm
		{
		jmp p[259*4];
		}
	}

// _PfTestPacket@20
extern "C" __declspec(naked) void __stdcall __E__260__()
	{
	__asm
		{
		jmp p[260*4];
		}
	}

// _PfUnBindInterface@4
extern "C" __declspec(naked) void __stdcall __E__261__()
	{
	__asm
		{
		jmp p[261*4];
		}
	}

// do_echo_rep
extern "C" __declspec(naked) void __stdcall __E__262__()
	{
	__asm
		{
		jmp p[262*4];
		}
	}

// do_echo_req
extern "C" __declspec(naked) void __stdcall __E__263__()
	{
	__asm
		{
		jmp p[263*4];
		}
	}

// if_indextoname
extern "C" __declspec(naked) void __stdcall __E__264__()
	{
	__asm
		{
		jmp p[264*4];
		}
	}

// if_nametoindex
extern "C" __declspec(naked) void __stdcall __E__265__()
	{
	__asm
		{
		jmp p[265*4];
		}
	}

// register_icmp
extern "C" __declspec(naked) void __stdcall __E__266__()
	{
	__asm
		{
		jmp p[266*4];
		}
	}

