module DateTime_extratools
   
    open System

    /// converts a date in UNIX POSIX format into DateTime
    let toDateTime (timestamp:int) =
        let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
        start.AddSeconds(float timestamp).ToLocalTime()

    // let unix_now = DateTimeOffset(DateTime.Now).ToUnixTimeSeconds() //val unix : int64 = 1488254491L
    //let dt =  DateTimeOffset.FromUnixTimeSeconds(unix)  //val dt : DateTimeOffset = 2017/02/28 4:01:31 +00:00
    //dt.LocalDateTime 

    let ToUnixTimeSeconds (d:DateTime) : int64 = 
        let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
        int64 (d - start).TotalSeconds


