Bloom-Revoke
============

Bloom-revoke is a server entity and browser extension that increases security of the web-browsing experience by periodically sending clients a bloom-filter of revoked TLS Certificates. This data is compiled and stored on our (school-owned) servers, and sent to browsers periodically - this provides a complete, up-to-date list of revoked certificates.

Why?
====

Not sure if you've heard... but TLS/SSL revokation is broken.

Revocation mechanisms today:

CRL (Certificate Revocation List):
----------------------------------
* This is a list managed by a CA (Certificate Authority) containing revoked certificates
* Certificates point to one of these, the client downloads it and checks if this cert has been revoked
> Why is this bad?
* The median size CRL is 51 KB, and you will need a certificate for every CA you are in contact with, so downloads are expensive

OCSP (Online Certificate Status Protocol):
------------------------------------------
* Instead of a list, CAs open up endpoints for the client to query when they visit a website. So you're going to google.com, you ask its CA: is google.com still legit?
* Certificates also point to one of these
> Why is this bad?
* Privacy risk: CAs know your site traffic history
* You pay for 2 requests, and you get 1 desired response :(

OCSP Stapling:
--------------
* The *server* makes an OCSP response, "staples" it to the cert, and sends it to you.
* An attacker can't change the CAs cert (hopefully), so it must include a valid one.
* If it's been revoked, the attacker can't help but include in its request that it's been revoked.
> Why is this bad?
* No one uses it. Really.

How about browsers?
===================

Abysmal!

Chrome
------
* Basically only does checking for EV certs
* Adds CRLSet
  - Must be really small (contains < 1% of revoked certs) ........
* Doesn't respect an OCSP Staple revokation   (wtf?)

Firefox
-------
- No CRLS!
- Only OCSP - reasonably effective but doesn't check intermediate / roots

Mobile
------
- No one checks anything because latency

Wow ok... so what should we do?
===============================

We propose that clients keep a *bloom-filter*, which is a probabilistic data structure, locally which contains domains that have been revoked.

Clients (browser extension) could update this list periodically
* it is very small and thus would have full coverage of every revoked certificate.
* it would be delivered out of band, so no added latency
* no privacy issues involved with OCSP
* no latency, so mobile browsers could do this!!! (******)
* bloom filters never have false negatives, so if you it's not in the bloom filter, it's definitely OK
* bloom filters sometimes have false positives, so if it's in the bloom filter, we have to check
  * this isn't that bad, as if it's really revoked the latency doesn't matter
  * if it isn't revoked, you incur a small penalty (but with OCSP, it's the same as you had before!)
* Shifting the option of security to the end-users, not servers or browser designers
