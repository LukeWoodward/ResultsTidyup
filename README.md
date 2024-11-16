# Results Tidyup

* [What is it?](#what-is-it)
* [Where is it?](#where-is-it)
* [How to use it?](#how-to-use-it)
* How to...
  * [Merge times on two timers](#merge-times-on-two-timers)
  * [Apply a scale factor to all times](#apply-a-scale-factor-to-all-times)
  * [Swap two ranges of tokens](#swap-two-ranges-of-tokens)
  * [Reverse a range of tokens](#reverse-a-range-of-tokens)
* [Problems](#problems)
* [What this doesn't do but may do in the future](#what-this-doesnt-do-but-may-do-in-the-future)
* [What this won't do](#what-this-wont-do)
* [What this isn't](#what-this-isnt)
* [Privacy/cookies notices](#privacy-cookies-notices)
* [Questions](#questions)

### What is it?

This is an attempt to help out parkrun volunteers doing results processing
who encounter problems with the results.  It's something that several times
I have wished I had available to me when I've been processing results myself.

It attempts to detect problems with timer and scanner files dropped into it,
and provide ways to fix some of the problems it identifies. If you need to,
you can then download modified files and upload them in to EMS.

### Where is it?

It can be found [here](http://lack-of.org.uk/results-tidyup).

### How to use it?

Before you can use Results Tidyup, you must first download from EMS any files
you wish to use with it. Do this by clicking the timer or scanner icon to
view the contents of the file in EMS. Then click Save. Save this somewhere
on your computer or phone.

You can then load the timer and/or scanner files into the application, using
the 'Upload files...' button at the top.  You can also drag files into the
browser window, but note that some browser antivirus plugins will block this.
Times should then appear on the left and barcodes scanned on the right.

You don't have to upload both timer and scanner files.  You can upload only
timer files, or upload only scanner files, or upload both.

You can either look at Results Tidyup to see what changes to make to the
results, or download the files from Results Tidyup and upload them back in
to EMS. Note that if you do the latter, you must deselect the original files
in EMS.

### How to...

#### Merge times on two timers

- Upload two timer files.
- If an offset is detected between the times on the two timers, choose to
  apply the offset to one timer.  (If this happens, it usually means that
  one of the timers was started late, so choose to add the offset to the
  slow timer.)
- Review the list of times in the list on the left.  Untick any times
  recorded on one timer that are incorrect.
- Download the merged list of times using the button to the right of the times.

#### Apply a scale factor to all times

Sometimes, courses may be found to be short, and a suitable correction
factor would need to be applied to all times to address for the short course.
You can either enter the scale factor yourself, or calculate it from the
expected distance and actual distance using the
[Riegel formula](https://en.wikipedia.org/wiki/Peter_Riegel#Race_time_prediction).

To do this:
- Upload one or two timer files.
- Click 'Timer Operations'
- Use the options in this window to specify the scale factor, or the expected
  and actual distances.
- Click 'Apply scale factor'.

#### Swap two ranges of tokens

A lot of parkruns have multiple 'sets' of tokens, and sometimes these are
given out in the wrong order.  For example, you may have tokens in sets of
100, i.e. 1-100, 101-200, 201-300, 301-400 and so on.  If, on one event,
the tokens were given out in the order 1-100, 101-200, **301-400**,
**201-300**, then you would need to swap over token ranges 201-300 and 301-400.
To do this:

- Upload all barcode scanner files.
- Click 'Token Operations...'.
- In the dialog that opens, click 'Swap token(s)' and enter 201-300 in one of
  the two fields and 301-400 in the other.
- Click 'Swap tokens'.
- Download all barcode data using the 'Download all scanned barcodes' button.
  This downloads one file containing all scanned barcodes.

#### Reverse a range of tokens

Occasionally, a set of tokens at a parkrun will be given out in the wrong
order.  For example, suppose your event has sets of tokens 1-25, 26-50, 51-75,
76-100 and so on, but the 51-75 set was given out in the wrong order, i.e.
they were given out in the order 1-25, 26-50, **75-51**, 76-100.  You need to
reverse the tokens in the range 51-75.  Sorting this out is similar to the
previous step:

- Upload all barcode scanner files.
- Click 'Token Operations...'.
- In the dialog that opens, click 'Reverse tokens' and enter 51-75 in the
  field.
- Click 'Reverse tokens'.
- Download all barcode data using the 'Download all scanned barcodes' button.
  This downloads one scanner file containing all scanned barcodes.

### Problems

Results Tidyup can identify various problems in the results and offer ways
to fix some of them. Any problems it identifies appear in the top-right of
the page.

Results Tidyup can identify the following problems with the files uploaded:

- Time offsets detected between the times in two timer files, indicating the
  timers weren't started at the same time.  You can fix this by adding or
  subtracting the detected time offset from all times on one of the timers.

- The same athlete barcode scanned with and without a finish token. EMS will
  report an error in the scanner file for an athlete-barcode scan without
  a corresponding finish-token scan, so you might think that this athlete
  had scanned in but won't get a result.  However, we can ignore the row
  without a finish token.

- The same athlete barcode scanned with multiple different finish tokens.
  Results Tidyup can't by itself tell which finish position is correct for
  that athlete: you will have to figure this out for yourself.  To help you,
  the athlete IDs link to the athlete's results history. If you've loaded
  one or more timer files, then the times recorded against the positions
  are also shown, allowing you to judge which of the finish positions is
  the more likely one.

- More than one athlete barcode scanned with the same finish token. Again,
  there's no obvious way to sort this out, but the links to the athlete
  results might help you identify which athlete was most likely to have
  finished with that finish token.

- Finish tokens scanned beyond the end of the list of times, e.g. there
  are 99 times but someone has scanned finish token 100, so the athlete
  in 100th place doesn't have a time. A time will need to be inserted,
  or a finish token/position removed, to correct this.

- Athlete barcode scans with no associated finish token.  (If the
  athlete barcode and finish token have been written down, you can
  double-click on the barcode-scanner row to add the missing information.)

#### Problems that come up less frequently with the Virtual Volunteer app

Development of this application started before the use of the Virtual
Volunteer app everywhere was made compulsory. Use of the Virtual Volunteer
app has removed a number of problems that had previously been encountered
with results processing, such as barcode scanner volunteers scanning athlete
barcodes and finish tokens the wrong way around, so the ability to detect
such problems has been removed. It's also a lot less likely that volunteers
will end up with practice scans left on their phone (I haven't seen this
happen since the Virtual Volunteer app became compulsory, but I saw it
happen several times before), so Results Tidyup no longer offers to set
the event start time and check for scans before it.

Results Tidyup still supports detecting mis-scanned barcodes and
unrecognised lines. I've seen the Opticon scanners misread a barcode and
generate nonsense barcodes such as `&d084`. In theory, the Virtual Volunteer
app *shouldn't* scan any barcode that isn't an athlete or finish-token
barcode, but I'll leave this check in just in case.

### What this doesn't do but may do in the future

This tool has so far been based on what I've seen happen when processing results.  Doubtless there are other things which Results Tidyup could include support for, such as:

- Handling three or more timers. (Do events often use three or more timers?)

### What this won't do

Results Tidyup will never magically fix all results-processing problems at an
event. It can detect a number of problems, but it never makes any corrections
if you don't tell it to. As a results processor you will always need to
understand what has gone on during the event and use your judgement to decide
what modifications to make to the results, if any.  Some modifications
suggested by Results Tidyup may in fact be incorrect.

Results Tidyup will never be able to look up athlete names from athlete
barcode numbers.  That would require access to the parkrun database but
for data-protection reasons we can't have that.

### What this isn't

This isn't an official parkrun tool.  If it doesn't do what you want, it's
not their fault. Don't contact them for support with it - they will not be
able to help!

This isn't guaranteed to be bug-free: I hope it is but I make no guarantees.

### Privacy/cookies notices

Results Tidyup runs entirely in your browser and doesn't send your data to
any servers.  Whilst this does impose some technical constraints, it helps
avoid any privacy issues.  I don't have to say what the servers that host
Results Tidyup are doing with your data, because they're not doing anything
with it at all.

Your browser also might say that Results Tidyup is "Not secure".  What this
means is that any data transmitted between your browser and the server isn't
encrypted and can be intercepted. As Results Tidyup doesn't send your data
to a server, there is no data transmitted to the server so no data that can
be intercepted in this way.

Results Tidyup does not use any cookies nor 'local storage' in your browser
to store any data.

## Questions

#### I click to download a file but nothing happens.  Why?

Are you using Safari on macOS?  File downloading is believed to not work
on this browser, although I don't own a Mac so I can't test this for myself.
Please use Firefox or Chrome instead.

I don't own a tablet computer so I don't know how these behave.

#### How does this compare to other stopwatch comparison tools?

Steve Megson's stopwatch comparison tool, at http://stopwatch.megson.org/,
is widely known among results processing teams and often used for merging
together two sets of timer times.

I can't say exactly how this tool compare its timer times, so I can't say
whether Results Tidyup is better or worse than it at comparing times. All
I can say is that it is different.

However, one particular difference regards handling timers not started at
the same time.  In this situation it can be useful to correct the offset
between the timers.

If the two timers are not started at the same time, Steve Megson's tool
and Results Tidyup can usually detect the difference. However, only Results
Tidyup gives you the option of which timer to apply the offset to and in
which direction: either adding it to all times on one or subtracting it
from all on the other. Steve Megson's tool automatically adds the offset
to all times recorded on the timer deemed to have been started later than
the other. Most of the time, when there is a difference between timer times
it is because one of them was started late, but on one occasion I have seen
a timer appear to be started early.

Steve Megson's tool can have its offset detection thrown off if the first
times recorded on the timers have a different offset.  I have two timer
files, with a time offset of two seconds between most times, but with an
extra three-second offset in the times recorded for the first finisher.
Steve Megson's tool doesn't detect a time offset for these files, whereas
Results Tidyup identifies the correct 2-second offset.

At the time of writing I am not aware of other stopwatch comparison tools.
