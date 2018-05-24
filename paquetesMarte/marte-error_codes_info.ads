------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                     'E r r o r _ C o d e s _ I n f o'
--
--                                   Spec
--
--
--  File 'error_codes_info.ads'                                  By MAR. and
--                                                                  Fguerreira
--
--  Description strings for the MaRTE error codes.
--
--  The list defined here is used by 'Kernel.Error_Code'. It is also
--  used by 'write_marte_c_headers.adb' to write automatically during
--  the MaRTE installation the files 'sys/marte_errno.h' and
--  'sys/marte_errno_info.h'. This assured the same values for the
--  error codes are used in both the Ada and C parts of the kernel.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------

with MaRTE.POSIX_Constants;

package MaRTE.Error_Codes_Info is

   pragma Pure;

   use MaRTE;

   type Error_Names is (NO_ERROR,
                        BAD_ADDRESS,
                        RESOURCE_TEMPORARILY_UNAVAILABLE,
                        RESOURCE_BUSY,
                        RESOURCE_DEADLOCK_AVOIDED,
                        INTERRUPTED_OPERATION,
                        INVALID_ARGUMENT,
                        NO_SUCH_PROCESS,
                        OPERATION_NOT_PERMITTED,
                        OPERATION_NOT_SUPPORTED,
                        OPERATION_NOT_IMPLEMENTED,
                        TIMED_OUT,
                        NOT_ENOUGH_SPACE,
                        RESULT_TOO_LARGE, -- Never produced in the Ada Kernel
                        APPSCHED_EVENT_MASKED,
                        APPSCHED_REJECTED,
                        APPSCHED_POLICY_ERROR,
                        TOO_MANY_OPEN_FILES,
                        FILENAME_TOO_LONG,
                        BAD_FILE_DESCRIPTOR,
                        NO_SUCH_FILE_OR_DIRECTORY,
                        PERMISSION_DENIED,
                        NO_ISR_ASSOCIATED
                        );

   type Error_Code_Info is record
      Code          : POSIX_Constants.Error_Code;
      C_Name        : String (1 .. 14);  -- POSIX.C name
      C_Description : String (1 .. 35);
   end record;

   List : constant array (Error_Names) of Error_Code_Info :=
     (NO_ERROR =>
        (POSIX_Constants.NO_ERROR,
         "NO_ERROR      ",
         "No error                           "),
      BAD_ADDRESS =>
        (POSIX_Constants.BAD_ADDRESS,
         "EFAULT        ",
         "Bad Address                        "),
      RESOURCE_TEMPORARILY_UNAVAILABLE =>
        (POSIX_Constants.RESOURCE_TEMPORARILY_UNAVAILABLE,
         "EAGAIN        ",
         "Rsrc temporarily unavailable       "),
      RESOURCE_BUSY =>
        (POSIX_Constants.RESOURCE_BUSY,
         "EBUSY         ",
         "Device busy                        "),
      RESOURCE_DEADLOCK_AVOIDED =>
        (POSIX_Constants.RESOURCE_DEADLOCK_AVOIDED,
         "EDEADLK       ",
         "Resource deadlock avoided          "),
      INTERRUPTED_OPERATION =>
        (POSIX_Constants.INTERRUPTED_OPERATION,
         "EINTR         ",
         "Interrupted system call            "),
      INVALID_ARGUMENT =>
        (POSIX_Constants.INVALID_ARGUMENT,
         "EINVAL        ",
         "Invalid argument                   "),
      NO_SUCH_PROCESS =>
        (POSIX_Constants.NO_SUCH_PROCESS,
         "ESRCH         ",
         "No such process                    "),
      OPERATION_NOT_PERMITTED =>
        (POSIX_Constants.OPERATION_NOT_PERMITTED,
         "EPERM         ",
         "Operation not permitted            "),
      OPERATION_NOT_SUPPORTED =>
        (POSIX_Constants.OPERATION_NOT_SUPPORTED,
         "ENOTSUP       ",
         "Not supported                      "),
      OPERATION_NOT_IMPLEMENTED =>
        (POSIX_Constants.OPERATION_NOT_IMPLEMENTED,
         "ENOSYS        ",
         "Operation not implemented          "),
      TIMED_OUT =>
        (POSIX_Constants.TIMED_OUT,
         "ETIMEDOUT     ",
         "Operation timed out                "),
      NOT_ENOUGH_SPACE =>
        (POSIX_Constants.NOT_ENOUGH_SPACE,
         "ENOMEM        ",
         "Not enough space                   "),
      RESULT_TOO_LARGE =>
        (POSIX_Constants.RESULT_TOO_LARGE,
         "ERANGE        ",
         "Result too large                   "),
      APPSCHED_EVENT_MASKED =>
        (POSIX_Constants.APPSCHED_EVENT_MASKED,
         "EMASKED       ",
         "Appsched event masked              "),
      APPSCHED_REJECTED =>
        (POSIX_Constants.APPSCHED_REJECTED,
         "EREJECT       ",
         "Appsched has rejected object       "),
      APPSCHED_POLICY_ERROR =>
        (POSIX_Constants.APPSCHED_POLICY_ERROR,
         "EPOLICY       ",
         "Not an appscheduler or bad policy  "),
      TOO_MANY_OPEN_FILES =>
        (POSIX_Constants.TOO_MANY_OPEN_FILES,
         "EMFILE        ",
         "Too many files open in the system  "),
      FILENAME_TOO_LONG =>
        (POSIX_Constants.FILENAME_TOO_LONG,
         "ENAMETOOLONG  ",
         "Lenght of path exceeds             "),
      BAD_FILE_DESCRIPTOR =>
        (POSIX_Constants.BAD_FILE_DESCRIPTOR,
         "EBADF         ",
         "Not a valid open file descriptor   "),
      NO_SUCH_FILE_OR_DIRECTORY =>
        (POSIX_Constants.NO_SUCH_FILE_OR_DIRECTORY,
         "ENOENT        ",
         "Specified pathname does not exist  "),
      PERMISSION_DENIED =>
        (POSIX_Constants.PERMISSION_DENIED,
         "EACCES        ",
         "Wrong attempt to access a file     "),
      NO_ISR_ASSOCIATED =>
        (POSIX_Constants.NO_ISR_ASSOCIATED,
         "ENOISR        ",
         "No ISR associated with thread      "));

end MaRTE.Error_Codes_Info;
