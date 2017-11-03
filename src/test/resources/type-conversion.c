#include <stdio.h>
#include <math.h>

int main() {
  float V1 = 1.99;
  int V2 = V1; // <true>

  float V3 = 2.87;
  int V4 = trunc(V3); // <false>


  int V5 = -1;
  unsigned int V6 = V5; // <false> maybe someday, but for now we don't know the value of V5


  int V7 = -1;
  unsigned int V8;
  if (V1 >= 0) {
    V8 = V7;
  } else {
    V8 = UINT_MAX + (V7 + 1); // <false>
  }

  char V9 = 54;  // <false>
  char V10 = V9; // <false> - Maybe one day
  V10 = 128;     // <true>

  int V11 = 288;       // <false>
  char V12 = V6 % 256; // <false>

  int V13 = 1, V14 = 2.3; // <true>

  (int) 2.3;   // <true>
  (int) 2;     // <false>
  (int *) 2.3; // <false> ignore all pointers
  (int *) 2;   // <false>
  (char *) 2;  // <false>

  char *c = 2; // <false>

  // crashed in /gcc/libgfortran/io/unix.c
  static const struct stream_vtable x = { y = 1 };
  static const struct stream_vtable x = {
    .read = (void *) raw_read
  };

  String s("abc"); // <false>
}


void F1(int A1) {}

void F0() {
  F1(2.1); // <true>
  F1(2);   // <false>
}

int F2() {
  return 1.2;  // <true>
}

int F3() {
  return 1;  // <false>
}

// linux/drivers/gpu/drm/msm/hdmi/hdmi_hdcp.c
struct hdmi_hdcp_reg_data {
	u32 reg_id;
	u32 off;
	char *name;
	u32 reg_val;
};

static int msm_hdmi_hdcp_transfer_v_h(struct hdmi_hdcp_ctrl *hdcp_ctrl)
{
	struct hdmi_hdcp_reg_data reg_data[]  = {
		{REG_HDMI_HDCP_RCVPORT_DATA7,  0x20, "V' H0"},
		{REG_HDMI_HDCP_RCVPORT_DATA8,  0x24, "V' H1"},
		{REG_HDMI_HDCP_RCVPORT_DATA9,  0x28, "V' H2"},
		{REG_HDMI_HDCP_RCVPORT_DATA10, 0x2C, "V' H3"},
		{REG_HDMI_HDCP_RCVPORT_DATA11, 0x30, "V' H4"},
	};
	u32 data[ARRAY_SIZE(reg_data)];
	int i;

  rc = msm_hdmi_ddc_read((u8 *)&data[i]); // ArrayIndexOutOfBoundsException

	return rc;
}
