# La Transparence et l'Obstacle 

# A Comprehensive and Harmonised European Banking Authority (EBA) Stress Test and Transparency Exercise Dataset

This tool and the information presented herein are independent and are not affiliated with, endorsed by, or sponsored by the EBA. The EBA has not commissioned, requested, or otherwise supported the creation of this tool. All data sourced from the EBA is publicly available and used in accordance with their respective terms of use. The author of this tool is solely responsible for its content, any analysis, interpretations, or conclusions drawn, and is not liable for any use of the data or information provided. While efforts have been made to ensure accuracy, the author cannot guarantee the completeness, reliability, or timeliness of the information. The information provided is for general informational purposes only and does not constitute legal or professional advice. Any issues, errors, or discrepancies encountered in this document or resulting from its use are the sole responsibility of the author and do not reflect upon the EBA.

Feel free to reach me out in case you experience any issue or find any mistake. 

<div class="centered">

### THE FRAMEWORK

</div>

The aim of this tool is to provide a an easy access to the data the EBA discloses on European banks as part as its ["Transparency Exercise"](https://eba.europa.eu/eu-wide-transparency-exercise-0), ["EU-Wide Stress-Testing Exercises"](https://eba.europa.eu/risk-and-data-analysis/risk-analysis/eu-wide-stress-testing), and Risk Parameters within its ["Risk Dashboard"](https://eba.europa.eu/risk-and-data-analysis/risk-analysis/risk-monitoring/risk-dashboard) framework. 

The Transparency Exercise was an annual exercise within which the EBA discloses balance-sheet information on banks linked to their solvency (capital adequacy), profitability, asset quality, and funding, with the aim to complement the Pillar 3 disclosures of banks. Since the Transparency Exercise has been discontinued in 2025, the dataset will not be updated after 2025 Q2. Data will still be available on the EBA ["Pillar 3 Data Hub"](https://edap-public.eba.europa.eu/Report/index/MTE1?rhversion=20260121164608-2) as part of the Pillar 3 disclosures of banks, although not for all items present in the Transparency Exercise. 

The EU-Wide stress-testing exercises have been initiated and coordinated by the EBA (CEBS before 2011) every two years overall since 2009. These forward-looking solvency exercises aim at assessing the resilience of financial institutions to adverse macroeconomic developments. As such, they contribute to the assessment of systemic risk in the European financial system. They are performed on a bottom-up fashion on a sample of banks representing at least 70% of total banking assets in each considered country. Since 2010, they have been followed by extensive releases of information on banks (solvency, asset quality, profitability), both realised data and results under the baseline and adverse scenarios. Available exercises are the 2010, 2011, 2014, 2016, 2018, 2021, 2023, and 2025 ones. As part of the [Supervisory Review and Evaluation Process (SREP)](https://www.bankingsupervision.europa.eu/activities/srep/html/index.en.html), the ECB also performs its own [stress-testing exercises](https://www.bankingsupervision.europa.eu/activities/stresstests/html/index.en.html) on less significant institutions. These exercises are followed by a high-level disclosure of data, available in 2021, 2023, and 2025. 

Eventually, the EBA Risk Dashboard is part of the regular risk assessment conducted by the EBA, which summarises the main risks and vulnerabilities in the banking sector in the EU. Within this dashboard, the Risk Parameters dataset presents the probabilities of default (PDs), loss rate and loss given default (LGDs) figures at the country level on a sample of financial institutions. These parameters are used to compute the risk weighted assets of banks. 

Four datasets are created from these four disclosures. 
* The Stress-Testing dataset (all stress-testing results since the 2010 one), available in the *Stress-Testing Exercises* tab;
* The SSM Stress-Testing dataset (all SSM stress-testing results since the 2021 one), available in the *Stress-Testing Exercises* tab;
* The Transparency Exercise dataset (all data points from the Transparency Exercise since the 2013 one), available in the *Transparency Exercise* tab;
* The Risk Parameters dataset, featuring default rates, loss rates, PDs and LGDs at the country level, available in the *Thematic Datasets* tab. 
All items are matched across exercises within each dataset. 

In addition, five transversal datasets are also available, in which items are matched both across years and exercises, using the days where there is an overlap in the transparency disclosures and the realised data of a stress-testing exercise. Items are grouped by category. All these datasets are available in the *Thematic Datasets* tab.

* The EBA PLC dataset gathers profits, losses and capital accounts from stress-testing results and transparency releases;
* The EBA Market dataset gathers market risk parameters from transparency releases;
* The EBA Exposures dataset gathers credit risk exposures from stress-testing results and transparency releases, with the exposures and countries where these exposures are generated;
* The EBA Sovereign dataset gathers the sovereign exposures of banks, from the Transparency Exercise disclosures;
* The EBA Sector exposure dataset, which gathers the exposure of banks to economic sectors, identified with their NACE codes.

Matching items in these datasets went with some simplification. In particular, F-IRB and A-IRB portfolios, distinct in stress-test results, have been merged to match the Transparency Exercise, which only reports total IRB and Standardised exposures. In addition, the "Exposure in Default" Standardised exposure has been created in stress-testing data to match Transparency releases. All the necessary information is available in the Metadata file, including all dictionaries and matches across frameworks and with the EBA original nomenclature. 

Ultimately, the Bank Distress Event Dataset lists major distress events of European banks from 2000 to end-2020, at a quarterly frequency. This dataset builds on the nomenclature presented in [Betz et al. (2014)](https://doi.org/10.1016/j.jbankfin.2013.11.041).

You can also find some [monetary policy calendars](https://github.com/thestresstester/La-Transparence-et-l-Obstacle/tree/main/Monetary%20Policy%20Calendars) and the [Transparency Exercise dashboard](https://github.com/thestresstester/La-Transparence-et-l-Obstacle/tree/main/Transparency%20Dashboard), an example use of Stress-Testing and Transparency Exercise data within this repository.  
