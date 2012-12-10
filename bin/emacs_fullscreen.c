/*
  Copyright (c) 2011, Alexander Manenko
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
  * Neither the name of the Alexander Manenko nor the
    names of its contributors may be used to endorse or promote products
    derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL ALEXANDER MANENKO BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*
  HOW TO USE
  ----------
  1. Add the following lines to your .emacs file:
  (defun switch-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
  (global-set-key [f11] 'switch-full-screen)

  2. Compile main.c to the folder in your PATH (I use Windows/system32).

  3. Now run Emacs and press F11 to switch into full-screen mode.
  Press F11 again to switch to windowed mode.
 */


#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <windows.h>
#include <shellapi.h>

#define CMD_TOPMOST L"--topmost"

#define EMACS_CLASS_NAME L"Emacs"

/* Windowed-mode styles for Emacs' window. */
#define EMACS_WINDOWED_STYLE (WS_CAPTION | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_SYSMENU | \
			      WS_THICKFRAME | WS_OVERLAPPED | WS_MINIMIZEBOX | WS_MAXIMIZEBOX)
#define EMACS_WINDOWED_STYLE_EX (WS_EX_LEFT | WS_EX_LTRREADING | WS_EX_RIGHTSCROLLBAR | WS_EX_ACCEPTFILES | \
				 WS_EX_WINDOWEDGE)

/* Full-screen-mode styles for Emacs' window. */
#define EMACS_FULLSCREEN_STYLE (WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_VISIBLE | WS_POPUP)
#define EMACS_FULLSCREEN_STYLE_EX 0x0

/* Parses command line and returns TRUE if it contains --topmost switch. */
BOOL is_topmost_requested()
{
  BOOL result = FALSE;
  LPWSTR *args = NULL;
  INT args_count = 0;
  INT i = 0;

  args = CommandLineToArgvW(GetCommandLineW(), &args_count);
  if (NULL == args)
    {
      /* Failed to get command line arguments. Assuming that there is no --topmost command line switch. */
    }
  else
    {
      /* Find --topmost command line switch. */
      for (i = 0; i < args_count; ++i)
	{
	  if (0 == _wcsicmp(args[i], CMD_TOPMOST))
	    {
	      /* Found. Exit from loop. */
	      result = TRUE;
	      break;
	    }
	    else
	      {
		/* Not found. Continue. */
	      }
	}
    }

  LocalFree(args);
  return result;
}

INT CALLBACK WinMain(HINSTANCE h_instance, HINSTANCE h_prev_instance, LPSTR cmd_line, int cmd_show)
{
  LONG current_style = 0; /* Current style of the emacs window. */
  LONG style = 0; /* New style of the emacs window. */
  LONG style_ex = 0; /* New extended style of the emacs window. */
  HWND insert_after = NULL;
  HWND emacs_window = FindWindowW(EMACS_CLASS_NAME, NULL);
  BOOL topmost_requested = is_topmost_requested();

  if (emacs_window)
    {
      /* If window is in full-screen mode, then it will be maximized. Clear WS_MAXIMIZE bit and then compare styles. */
      current_style = GetWindowLongPtrW(emacs_window, GWL_STYLE);
      current_style ^= WS_MAXIMIZE;
      /* We will toggle between full-screen/windowed modes for Emacs */
      if (current_style == EMACS_FULLSCREEN_STYLE)
	{
	  style = EMACS_WINDOWED_STYLE;
	  style_ex = EMACS_WINDOWED_STYLE_EX;
	  if (topmost_requested)
	    {
	      style_ex |= WS_EX_TOPMOST;
	      insert_after = HWND_NOTOPMOST;
	    }
	}
      else
	{
	  style = EMACS_FULLSCREEN_STYLE;
	  style_ex = EMACS_FULLSCREEN_STYLE_EX;
	  if (topmost_requested)
	    {
	      style_ex |= WS_EX_TOPMOST;
	      insert_after = HWND_TOPMOST;
	    }
	}

      SetWindowLongPtrW(emacs_window, GWL_STYLE, style);
      SetWindowLongPtrW(emacs_window, GWL_EXSTYLE, style_ex);
      SetWindowPos(emacs_window, insert_after, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE);
      ShowWindowAsync(emacs_window, SW_MAXIMIZE);
    }
  else
    {
      MessageBoxW(NULL, L"Can not find emacs window.", L"Error", MB_OK | MB_ICONERROR);
    }

  return 0;
}
