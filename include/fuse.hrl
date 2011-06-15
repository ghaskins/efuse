-define(API_MAJOR, 7).
-define(API_MINOR, 16).


-define(FUSE_LOOKUP	   , 1).
-define(FUSE_FORGET	   , 2).
-define(FUSE_GETATTR	   , 3).
-define(FUSE_SETATTR	   , 4).
-define(FUSE_READLINK	   , 5).
-define(FUSE_SYMLINK	   , 6).
-define(FUSE_MKNOD	   , 8).
-define(FUSE_MKDIR	   , 9).
-define(FUSE_UNLINK	   , 10).
-define(FUSE_RMDIR	   , 11).
-define(FUSE_RENAME	   , 12).
-define(FUSE_LINK	   , 13).
-define(FUSE_OPEN	   , 14).
-define(FUSE_READ	   , 15).
-define(FUSE_WRITE	   , 16).
-define(FUSE_STATFS	   , 17).
-define(FUSE_RELEASE       , 18).
-define(FUSE_FSYNC         , 20).
-define(FUSE_SETXATTR      , 21).
-define(FUSE_GETXATTR      , 22).
-define(FUSE_LISTXATTR     , 23).
-define(FUSE_REMOVEXATTR   , 24).
-define(FUSE_FLUSH         , 25).
-define(FUSE_INIT          , 26).
-define(FUSE_OPENDIR       , 27).
-define(FUSE_READDIR       , 28).
-define(FUSE_RELEASEDIR    , 29).
-define(FUSE_FSYNCDIR      , 30).
-define(FUSE_GETLK         , 31).
-define(FUSE_SETLK         , 32).
-define(FUSE_SETLKW        , 33).
-define(FUSE_ACCESS        , 34).
-define(FUSE_CREATE        , 35).
-define(FUSE_INTERRUPT     , 36).
-define(FUSE_BMAP          , 37).
-define(FUSE_DESTROY       , 38).
-define(FUSE_IOCTL         , 39).
-define(FUSE_POLL          , 40).
-define(FUSE_NOTIFY_REPLY  , 41).
-define(FUSE_BATCH_FORGET  , 42).

-define(S_IFMT    , 8#170000). % bit mask for the file type bit fields
-define(S_IFSOCK  , 8#140000). % socket
-define(S_IFLNK   , 8#120000). % symbolic link
-define(S_IFREG   , 8#100000). % regular file
-define(S_IFBLK   , 8#060000). % block device
-define(S_IFDIR   , 8#040000). % directory
-define(S_IFCHR   , 8#020000). % character device
-define(S_IFIFO   , 8#010000). % FIFO
-define(S_ISUID   , 8#004000). % set UID bit
-define(S_ISGID   , 8#002000). % set-group-ID bit (see below)
-define(S_ISVTX   , 8#001000). % sticky bit (see below)
-define(S_IRWXU   , 8#000700). % mask for file owner permissions
-define(S_IRUSR   , 8#000400). % owner has read permission
-define(S_IWUSR   , 8#000200). % owner has write permission
-define(S_IXUSR   , 8#000100). % owner has execute permission
-define(S_IRWXG   , 8#000070). % mask for group permissions
-define(S_IRGRP   , 8#000040). % group has read permission
-define(S_IWGRP   , 8#000020). % group has write permission
-define(S_IXGRP   , 8#000010). % group has execute permission
-define(S_IRWXO   , 8#000007). % mask for permissions for others (not in group)
-define(S_IROTH   , 8#000004). % others have read permission
-define(S_IWOTH   , 8#000002). % others have write permission
-define(S_IXOTH   , 8#000001). % others have execute permission

-define(O_ACCMODE ,	8#00003).
-define(O_RDONLY  ,	8#00000).
-define(O_WRONLY  ,	8#00001).
-define(O_RDWR	  ,	8#00002).
-define(O_CREAT	  ,	8#00100).
-define(O_EXCL	  ,	8#00200).
-define(O_NOCTTY  ,	8#00400).
-define(O_TRUNC	  ,	8#01000).
-define(O_APPEND  ,	8#02000).
-define(O_NONBLOCK,	8#04000).
-define(O_NDELAY  ,	O_NONBLOCK).
-define(O_SYNC	  ,	8#10000).
-define(O_FSYNC	  ,	 O_SYNC).
-define(O_ASYNC	  ,	8#20000).

-define(IN_HEADER_SIZE, 40).
-define(OUT_HEADER_SIZE, 16).
-record(in_header, {len, opcode, unique, nodeid, uid, gid, pid}).
-record(out_header, {len, error, unique}).

-define(INIT_IN_SIZE, 16).
-record(init_in, {major, minor, max_readahead, flags}).
-record(init_out, {major, minor, max_readahead=0, flags=0,
		   max_background=0, congestion_threshold=0, max_write=0}).

-record(kstatfs, {blocks=0, bfree=0, bavail=0, files=0,
		  ffree=0, bsize=0, namelen=0, frsize=0}).

-record(attr, {ino, size=0, blocks=0,
	       atime=0, mtime=0, ctime=0,
	       atimensec=0, mtimensec=0, ctimensec=0,
	       mode=0, nlink=0, uid=0, gid=0, rdev=0, blksize=0}).

-record(getattr_in, {flags, fh}).
-record(attr_out, {timeout=1, attr=#attr{}}).

-record(open_in, {flags}).
-record(open_out, {fh=0, flags=0}).

-record(read_in, {fh, offset, size, read_flags, lock_owner, flags}).

-record(release_in, {fh, flags, release_flags, lock_owner}).

-record(dirent, {ino, name, type=0}).

-record(entry_out, {ino, generation=0, attr_timeout=1,
		    entry_timeout=1, attr=#attr{}}).
