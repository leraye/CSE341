(*1*)
fun is_older (d1: int * int * int, d2: int * int * int) =
    if #3 d1 < #3 d2
    then true
    else if #3 d1 = #3 d2 andalso #2 d1 < #2 d2
    then true
    else if #3 d1 = #3 d2 andalso #2 d1 = #2 d2 andalso #1 d1 < #1 d2
    then true
    else false

val d1 = (6, 6, 1944);
val d2 = (26, 12, 1991);
val x = is_older(d1, d2);

(*2*)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else
	let val rest = number_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then 1 + rest
	    else rest
	end
	    
val dates = [];
val z = number_in_month(dates, 12);

(*3*)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

val dates = [(6, 6, 1944), (2, 2, 1943), (7, 6, 1942), (7, 12, 1941), (10, 1, 2016), (17, 4, 1961), (5, 1, 1968)];
val months = [1, 2, 3];
val z = number_in_months(dates, months);

(*4*)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else
	let val rest = dates_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then (hd dates) :: rest
	    else rest
	end
	    
val dates = [(6, 6, 1944), (2, 2, 1943), (7, 6, 1942), (7, 12, 1941), (10, 1, 2016), (17, 4, 1961), (5, 1, 1968)];
val z = dates_in_month(dates, 12);

(*5*)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

val dates = [(6, 6, 1944), (2, 2, 1943), (7, 6, 1942), (7, 12, 1941), (10, 1, 2016), (17, 4, 1961), (5, 1, 1968)];
val z = dates_in_months(dates, [12, 1, 2]);

(*6*)
fun get_nth (l, n) =
    if null l
    then NONE
    else if n = 1
    then SOME (hd l)
    else get_nth(tl l, n - 1)

val L = [9, 7, 3, 9, 0, 5, 2];
val n = 14;
val z = get_nth(L, n);

(*7*)
val Months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
fun date_to_string (date: int * int * int) =
    let val m = get_nth(Months, #2 date)
    in
	if isSome (m)
	then valOf (m) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
	else ""
    end

val d = (11, 9, 2001);
val z = date_to_string(d);

(*8*)
fun number_before_reaching_sum (sum: int, l: int list) =
    if null l orelse sum <= 0
    then
	if sum > 0
	then 0
        else ~1
    else 1 + number_before_reaching_sum(sum - hd l, tl l);

val L = [7,1,5,3,6,4];
val s = 100;
val z = number_before_reaching_sum(s, L);

(*9*)
val MonthDay = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
fun what_month (day: int) =
    let
	val d = number_before_reaching_sum(day, MonthDay)
	val m = get_nth(Months, d + 1)
    in
	if isSome m
        then valOf m
        else ""
    end
val z = what_month(360);

(*10*)
fun month_range (d1: int, d2: int) =
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1 + 1, d2);

val z = month_range(69, 120);

(*11*)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let val rest = oldest(tl dates)
	in
	    if isSome rest
	    then
		if is_older(hd dates, valOf rest)
		then SOME (hd dates)
		else rest
	    else SOME (hd dates)
	end;

val dates = [(6, 6, 1944), (2, 2, 1943), (7, 6, 1942), (7, 12, 1941), (10, 1, 2016), (17, 4, 1961), (5, 1, 1968)];
val z = oldest(dates);

(*12*)
fun cumulative_sum (nums: int list) =
    let fun accumulate (lst: int list, base: int) =
	    if null lst
	    then []
	    else
		let val b = hd lst + base
		in
		    b :: accumulate(tl lst, b)
		end
    in
	accumulate(nums, 0)
    end;

val z = cumulative_sum([12, 27, 13]);

(*13*)
fun is_in (num: int, lst: int list) =
    if null lst
    then false
    else num = (hd lst) orelse is_in(num, tl lst);

val L = [9, 3, 6, 7, 10];
val z = is_in(8, L);
val z = is_in(6, L);

fun remove_duplicates (nums: int list) =
    if null nums
    then []
    else
	let val rest = remove_duplicates(tl nums)
	in
	    if is_in(hd nums, tl nums)
	    then rest
	    else (hd nums) :: rest
	end

val L = [9, 3, 0, 9, 9, 10, 4, 2, 3, 1];
val z = remove_duplicates(L)

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    let val m = remove_duplicates(months)
    in
	number_in_months(dates, m)
    end;

val dates = [(6, 6, 1944), (2, 2, 1943), (7, 6, 1942), (7, 12, 1941), (10, 1, 2016), (17, 4, 1961), (5, 1, 1968), (11, 9, 2001), (9, 11, 1989)];
val months = [9, 3, 0, 9, 9, 10, 4, 2, 3, 1];
val z = number_in_months_challenge(dates, months);

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    let val m = remove_duplicates(months)
    in
	dates_in_months(dates, m)
    end;
val z = dates_in_months_challenge(dates, months);

(*14*)
fun reasonable_date (date: int * int * int) =
    let val y = #3 date
	val m = #2 date
	val d = #1 date
	val num_of_days = valOf (get_nth (MonthDay, m))
    in
	if y > 0 andalso m > 0 andalso m < 13
	then
	    if (y mod 400 = 0 orelse (y mod 100 > 0 andalso y mod 4 = 0)) andalso m = 2
	    then
		if 1 <= d andalso num_of_days + 1 >= d
		then true
		else false
	    else
		if 1 <= d andalso num_of_days >= d
		then true
		else false
	else
	    false
    end

val x = (9, 11, 2000);
val z = reasonable_date(x);
val x = (29, 2, 2001);
val z = reasonable_date(x);
val x = (29, 2, 2020);
val z = reasonable_date(x);
val x = (29, 2, 2200);
val z = reasonable_date(x);
	    
		    
