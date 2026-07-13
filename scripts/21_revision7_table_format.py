import math
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
MAIN_DIR = ROOT / "submission" / "tables" / "main"
SUPP_DIR = ROOT / "submission" / "tables" / "supplementary"


def fmt_p(v: float) -> str:
    if pd.isna(v):
        return ""
    if v < 0.001:
        return "<0.001"
    return f"{v:.3f}"


def fmt_arr(estimate: float, low: float, high: float) -> str:
    return f"{estimate:.2f} ({low:.2f}-{high:.2f})"


def fmt_rate_ci(estimate: float, low: float, high: float) -> str:
    return f"{estimate:.1f} ({low:.1f}-{high:.1f})"


def outcome_label(x: str) -> str:
    mapping = {
        "preterm_birth": "Preterm birth",
        "low_birth_weight": "Low birth weight",
        "term_low_birth_weight": "Term low birth weight",
    }
    return mapping.get(x, x)


def country_label(x: str) -> str:
    return {"United States": "United States", "Brazil": "Brazil"}.get(x, x)


def make_table1():
    t1 = pd.read_csv(MAIN_DIR / "table1_singleton_baseline.csv")
    if "country" not in t1.columns:
        return
    t1 = t1.set_index("country")
    rows = [
        ("Singleton births, n", "births", "n"),
        ("Maternal age, mean years", "maternal_age_mean", "1f"),
        ("Age risk, %", "age_risk_pct", "1f"),
        ("Low education, %", "low_education_pct", "1f"),
        ("Low prenatal-visit count (<4), %", "inadequate_prenatal_care_pct", "1f"),
        ("No prenatal care, %", "no_prenatal_care_pct", "1f"),
        ("Male newborn, %", "male_pct", "1f"),
        ("Preterm birth, %", "preterm_birth_pct", "1f"),
        ("Low birth weight, %", "low_birth_weight_pct", "1f"),
        ("Very preterm birth, %", "very_preterm_birth_pct", "1f"),
        ("Very low birth weight, %", "very_low_birth_weight_pct", "1f"),
        ("Cesarean delivery, %", "cesarean_delivery_pct", "1f"),
    ]
    out = []
    for label, col, typ in rows:
        br = t1.loc["Brazil", col]
        us = t1.loc["United States", col]
        if typ == "n":
            br_v = f"{int(round(br)):,}"
            us_v = f"{int(round(us)):,}"
        else:
            br_v = f"{float(br):.1f}"
            us_v = f"{float(us):.1f}"
        out.append({"Characteristic": label, "Brazil": br_v, "United States": us_v})
    pd.DataFrame(out).to_csv(MAIN_DIR / "table1_singleton_baseline.csv", index=False)


def make_table2():
    t2 = pd.read_csv(MAIN_DIR / "table2_singleton_outcome_rates.csv")
    if "births" not in t2.columns and "note" in t2.columns:
        t2 = t2.drop(columns=["note"])
        t2.to_csv(MAIN_DIR / "table2_singleton_outcome_rates.csv", index=False)
        return
    if "births" not in t2.columns:
        return
    out = t2.copy()
    out = out.rename(columns={"births": "singleton_births"})
    out["singleton_births"] = out["singleton_births"].round().astype(int).map(lambda x: f"{x:,}")
    rate_cols = [c for c in out.columns if c.endswith("_rate_per_1000")]
    for c in rate_cols:
        out[c] = out[c].astype(float).map(lambda x: f"{x:.1f}")
    out.to_csv(MAIN_DIR / "table2_singleton_outcome_rates.csv", index=False)


def make_table3():
    t3 = pd.read_csv(MAIN_DIR / "table3_singleton_risk_score_models.csv")
    if "country" not in t3.columns and "P for interaction" in t3.columns:
        t3 = t3.drop(columns=["P for interaction"])
        t3.to_csv(MAIN_DIR / "table3_singleton_risk_score_models.csv", index=False)
        return
    if "country" not in t3.columns:
        return
    inter = pd.read_csv(SUPP_DIR / "Supplementary_Table_2_country_interaction_ratios.csv")
    use_terms = ["risk_score3_cat1", "risk_score3_cat2", "risk_score3_cat3"]
    use_outcomes = ["preterm_birth", "low_birth_weight"]
    rows = []
    for outcome in use_outcomes:
        # reference row
        rows.append(
            {
                "Outcome": outcome_label(outcome),
                "Risk score": "0 domains",
                "Brazil aRR (95% CI)": "Reference",
                "United States aRR (95% CI)": "Reference",
                "Ratio of aRRs, US vs Brazil (95% CI)": "—",
                "Model N (Brazil)": f"{int(t3[(t3.country == 'Brazil') & (t3.outcome == outcome)]['n'].iloc[0]):,}",
                "Model N (United States)": f"{int(t3[(t3.country == 'United States') & (t3.outcome == outcome)]['n'].iloc[0]):,}",
                "Model N (Pooled)": f"{int(t3[(t3.country == 'Pooled') & (t3.outcome == outcome)]['n'].iloc[0]):,}",
            }
        )
        for idx, term in enumerate(use_terms, start=1):
            br = t3[(t3.country == "Brazil") & (t3.outcome == outcome) & (t3.term == term)].iloc[0]
            us = t3[(t3.country == "United States") & (t3.outcome == outcome) & (t3.term == term)].iloc[0]
            ir = inter[(inter.outcome == outcome) & (inter.risk_score.astype(str) == str(idx))].iloc[0]
            rows.append(
                {
                    "Outcome": outcome_label(outcome),
                    "Risk score": f"{idx} domain" if idx == 1 else f"{idx} domains",
                    "Brazil aRR (95% CI)": fmt_arr(br["estimate"], br["conf.low"], br["conf.high"]),
                    "United States aRR (95% CI)": fmt_arr(us["estimate"], us["conf.low"], us["conf.high"]),
                    "Ratio of aRRs, US vs Brazil (95% CI)": fmt_arr(
                        ir["ratio_of_aRR_US_vs_Brazil"], ir["conf.low"], ir["conf.high"]
                    ),
                    "Model N (Brazil)": f"{int(br['n']):,}",
                    "Model N (United States)": f"{int(us['n']):,}",
                    "Model N (Pooled)": f"{int(t3[(t3.country == 'Pooled') & (t3.outcome == outcome)]['n'].iloc[0]):,}",
                }
            )
    pd.DataFrame(rows).to_csv(MAIN_DIR / "table3_singleton_risk_score_models.csv", index=False)


def make_table4():
    old_path = MAIN_DIR / "table4_singleton_phenotype_models.csv"
    new_path = MAIN_DIR / "table4_singleton_risk_profile_models.csv"
    supp14_path = SUPP_DIR / "Supplementary_Table_14_risk_profile_interaction_p_values.csv"
    input_path = new_path if new_path.exists() else old_path
    t4 = pd.read_csv(input_path)
    if "country" not in t4.columns and "P for interaction" in t4.columns:
        supp14 = t4[
            ["Outcome", "Risk profile", "Ratio of aRRs, US vs Brazil (95% CI)", "P for interaction"]
        ].copy()
        supp14 = supp14[supp14["Risk profile"] != "Low risk"].copy()
        supp14.to_csv(supp14_path, index=False)
        t4 = t4.drop(columns=["P for interaction"])
        t4.to_csv(new_path, index=False)
        return
    if "country" not in t4.columns:
        return
    profile_terms = [
        ("low_risk", "Low risk"),
        ("age_only", "Age risk only"),
        ("inadequate_care_only", "Low prenatal-visit count only"),
        ("low_education_only", "Low education only"),
        ("age_inadequate", "Age risk + low visit count"),
        ("age_low_education", "Age risk + low education"),
        ("education_inadequate", "Low education + low visit count"),
        ("all_three", "All three domains"),
    ]
    outcomes = ["preterm_birth", "low_birth_weight"]
    rows = []
    interaction_rows = []
    for outcome in outcomes:
        pooled = t4[(t4.country == "Pooled") & (t4.outcome == outcome)]
        for raw, label in profile_terms:
            if raw == "low_risk":
                br_n = int(t4[(t4.country == "Brazil") & (t4.outcome == outcome)]["n"].iloc[0])
                us_n = int(t4[(t4.country == "United States") & (t4.outcome == outcome)]["n"].iloc[0])
                po_n = int(t4[(t4.country == "Pooled") & (t4.outcome == outcome)]["n"].iloc[0])
                rows.append(
                    {
                        "Outcome": outcome_label(outcome),
                        "Risk profile": label,
                        "Brazil aRR (95% CI)": "Reference",
                        "United States aRR (95% CI)": "Reference",
                        "Ratio of aRRs, US vs Brazil (95% CI)": "—",
                        "Model N (Brazil)": f"{br_n:,}",
                        "Model N (United States)": f"{us_n:,}",
                        "Model N (Pooled)": f"{po_n:,}",
                    }
                )
                continue
            br = t4[(t4.country == "Brazil") & (t4.outcome == outcome) & (t4.term == f"risk_phenotype3{raw}")].iloc[0]
            us = t4[
                (t4.country == "United States") & (t4.outcome == outcome) & (t4.term == f"risk_phenotype3{raw}")
            ].iloc[0]
            ir = pooled[pooled.term == f"risk_phenotype3{raw}:countryUnited States"].iloc[0]
            rows.append(
                {
                    "Outcome": outcome_label(outcome),
                    "Risk profile": label,
                    "Brazil aRR (95% CI)": fmt_arr(br["estimate"], br["conf.low"], br["conf.high"]),
                    "United States aRR (95% CI)": fmt_arr(us["estimate"], us["conf.low"], us["conf.high"]),
                    "Ratio of aRRs, US vs Brazil (95% CI)": fmt_arr(ir["estimate"], ir["conf.low"], ir["conf.high"]),
                    "Model N (Brazil)": f"{int(br['n']):,}",
                    "Model N (United States)": f"{int(us['n']):,}",
                    "Model N (Pooled)": f"{int(pooled['n'].iloc[0]):,}",
                }
            )
            interaction_rows.append(
                {
                    "Outcome": outcome_label(outcome),
                    "Risk profile": label,
                    "Ratio of aRRs, US vs Brazil (95% CI)": fmt_arr(ir["estimate"], ir["conf.low"], ir["conf.high"]),
                    "P for interaction": fmt_p(ir["p.value"]),
                }
            )
    pd.DataFrame(rows).to_csv(new_path, index=False)
    pd.DataFrame(interaction_rows).to_csv(supp14_path, index=False)


def make_table5():
    t5 = pd.read_csv(MAIN_DIR / "table5_absolute_risks_with_ci.csv")
    if "exposure" not in t5.columns:
        return
    exposure_map = {
        "low_risk": "Low risk",
        "age_only": "Age risk only",
        "inadequate_care_only": "Low prenatal-visit count only",
        "low_education_only": "Low education only",
        "age_inadequate": "Age risk + low visit count",
        "age_low_education": "Age risk + low education",
        "education_inadequate": "Low education + low visit count",
        "all_three": "All three domains",
    }
    rows = []
    for _, r in t5.iterrows():
        risk_txt = fmt_rate_ci(r["adjusted_risk_per_1000"], r["risk_ci_low"], r["risk_ci_high"])
        if r["exposure"] == "low_risk":
            diff_txt = "Reference"
        else:
            diff_txt = fmt_rate_ci(r["risk_difference_per_1000"], r["risk_difference_ci_low"], r["risk_difference_ci_high"])
        rows.append(
            {
                "Country": country_label(r["country"]),
                "Outcome": outcome_label(r["outcome"]),
                "Risk profile": exposure_map.get(r["exposure"], r["exposure"]),
                "Adjusted risk per 1,000 (95% CI)": risk_txt,
                "Risk difference per 1,000 (95% CI)": diff_txt,
            }
        )
    pd.DataFrame(rows).to_csv(MAIN_DIR / "table5_absolute_risks_with_ci.csv", index=False)


def make_supp1():
    p = SUPP_DIR / "Supplementary_Table_1_missingness.csv"
    df = pd.read_csv(p)
    if "births" in df.columns:
        df = df.rename(columns={"births": "singleton_births"})
    for c in [x for x in df.columns if x.endswith("_pct")]:
        df[c] = df[c].astype(float).map(lambda x: f"{x:.2f}")
    df["singleton_births"] = (
        df["singleton_births"].astype(str).str.replace(",", "", regex=False).astype(float).round().astype(int).map(lambda x: f"{x:,}")
    )
    df.to_csv(p, index=False)


def make_supp2():
    p = SUPP_DIR / "Supplementary_Table_2_country_interaction_ratios.csv"
    df = pd.read_csv(p)
    if "outcome" not in df.columns:
        return
    df["Outcome"] = df["outcome"].map(outcome_label)
    df["Risk score"] = df["risk_score"].astype(int).astype(str).map(lambda x: f"{x} domain" if x == "1" else f"{x} domains")
    df["Ratio of aRRs, US vs Brazil (95% CI)"] = df.apply(
        lambda r: fmt_arr(r["ratio_of_aRR_US_vs_Brazil"], r["conf.low"], r["conf.high"]), axis=1
    )
    df["P for interaction"] = df["p_interaction"].astype(float).map(fmt_p)
    out = df[["Outcome", "Risk score", "Ratio of aRRs, US vs Brazil (95% CI)", "P for interaction"]]
    out.to_csv(p, index=False)


def _filter_model_table(path: Path, keep_terms: list[str], risk_col_name: str, risk_map: dict[str, str]):
    df = pd.read_csv(path)
    if "term" not in df.columns:
        return
    df = df[df["term"].isin(keep_terms) & df["country"].isin(["Brazil", "United States"])].copy()
    df[risk_col_name] = df["term"].map(risk_map)
    df["Outcome"] = df["outcome"].map(outcome_label)
    df["Country"] = df["country"].map(country_label)
    df["aRR (95% CI)"] = df.apply(lambda r: fmt_arr(r["estimate"], r["conf.low"], r["conf.high"]), axis=1)
    df["P value"] = df["p.value"].astype(float).map(fmt_p)
    df["Model N"] = df["n"].astype(int).map(lambda x: f"{x:,}")
    out = df[["Country", "Outcome", risk_col_name, "aRR (95% CI)", "P value", "Model N"]]
    out = out.sort_values(["Outcome", "Country", risk_col_name]).reset_index(drop=True)
    out.to_csv(path, index=False)


def make_supp4_5_7_8():
    _filter_model_table(
        SUPP_DIR / "Supplementary_Table_4_term_low_birth_weight.csv",
        ["risk_score3_cat1", "risk_score3_cat2", "risk_score3_cat3"],
        "Risk score",
        {
            "risk_score3_cat1": "1 domain",
            "risk_score3_cat2": "2 domains",
            "risk_score3_cat3": "3 domains",
        },
    )
    _filter_model_table(
        SUPP_DIR / "Supplementary_Table_5_age_subtype_models.csv",
        [
            "age_subtypeteenage_mother",
            "age_subtypeadvanced_maternal_age_35_39",
            "age_subtypevery_advanced_maternal_age",
        ],
        "Age subtype",
        {
            "age_subtypeteenage_mother": "Teenage mother (<20 years)",
            "age_subtypeadvanced_maternal_age_35_39": "Advanced maternal age (35-39 years)",
            "age_subtypevery_advanced_maternal_age": "Very advanced maternal age (>=40 years)",
        },
    )
    _filter_model_table(
        SUPP_DIR / "Supplementary_Table_7_no_prenatal_care_sensitivity.csv",
        ["risk_score_no_care_cat1", "risk_score_no_care_cat2", "risk_score_no_care_cat3"],
        "Risk score (no prenatal care domain)",
        {
            "risk_score_no_care_cat1": "1 domain",
            "risk_score_no_care_cat2": "2 domains",
            "risk_score_no_care_cat3": "3 domains",
        },
    )
    _filter_model_table(
        SUPP_DIR / "Supplementary_Table_8_age_education_only_sensitivity.csv",
        ["risk_score_age_education_cat1", "risk_score_age_education_cat2"],
        "Risk score (age + education domains)",
        {
            "risk_score_age_education_cat1": "1 domain",
            "risk_score_age_education_cat2": "2 domains",
        },
    )


def make_supp6():
    out = pd.DataFrame(
        [
            {
                "harmonized_category": "Low education",
                "us_source_variable": "meduc",
                "us_source_coding_detail": "Codes 1-2: 8th grade or less; 9th-12th grade with no diploma",
                "brazil_source_variable": "ESCMAE2010",
                "brazil_source_coding_detail": "Codes 0-2: no schooling; fundamental I; fundamental II",
                "final_decision": "Used as registry marker approximating less than completed high school or country-specific secondary schooling.",
            },
            {
                "harmonized_category": "Middle education",
                "us_source_variable": "meduc",
                "us_source_coding_detail": "Codes 3-6: high school/GED; some college; associate degree",
                "brazil_source_variable": "ESCMAE2010",
                "brazil_source_coding_detail": "Codes 3-4: secondary education; incomplete higher education",
                "final_decision": "Used as intermediate education marker.",
            },
            {
                "harmonized_category": "High education",
                "us_source_variable": "meduc",
                "us_source_coding_detail": "Codes 7-8: bachelor's degree; master's/professional/doctorate degree",
                "brazil_source_variable": "ESCMAE2010",
                "brazil_source_coding_detail": "Code 5: completed higher education",
                "final_decision": "Used as highest education marker.",
            },
            {
                "harmonized_category": "Unknown",
                "us_source_variable": "meduc",
                "us_source_coding_detail": "Not stated, blank, or non-reporting",
                "brazil_source_variable": "ESCMAE / ESCMAE2010",
                "brazil_source_coding_detail": "Ignored/unknown/missing categories",
                "final_decision": "Retained as unknown; never recoded as low risk.",
            },
        ]
    )
    out.to_csv(SUPP_DIR / "Supplementary_Table_6_education_harmonization.csv", index=False)


def make_supp9():
    old_path = SUPP_DIR / "Supplementary_Table_9_phenotype_prevalence.csv"
    new_path = SUPP_DIR / "Supplementary_Table_9_risk_profile_prevalence.csv"
    input_path = new_path if new_path.exists() else old_path
    df = pd.read_csv(input_path)
    if "country" not in df.columns:
        return
    profile_col = "risk_phenotype3" if "risk_phenotype3" in df.columns else "Risk profile"
    order = [
        "Low risk",
        "Age risk only",
        "Low prenatal-visit count only",
        "Low education only",
        "Age risk + low visit count",
        "Age risk + low education",
        "Low education + low visit count",
        "All three domains",
    ]
    df[profile_col] = pd.Categorical(df[profile_col], categories=order, ordered=True)
    df = df.sort_values(["country", profile_col]).copy()
    df["births"] = df["births"].astype(str).str.replace(",", "", regex=False).astype(int).map(lambda x: f"{x:,}")
    df["total_births"] = df["total_births"].astype(str).str.replace(",", "", regex=False).astype(int).map(lambda x: f"{x:,}")
    df["percent"] = df["percent"].astype(float).map(lambda x: f"{x:.1f}")
    df["denominator_note"] = (
        "Profile-classifiable singleton births only; records with missing profile-defining variables were excluded."
    )
    df = df.rename(
        columns={
            "country": "Country",
            profile_col: "Risk profile",
            "births": "Births",
            "total_births": "Total births",
            "percent": "Percent",
            "denominator_note": "Denominator note",
        }
    )
    df[["Country", "Risk profile", "Births", "Total births", "Percent", "Denominator note"]].to_csv(new_path, index=False)


def make_supp10():
    p = SUPP_DIR / "Supplementary_Table_10_cross_national_standardization.csv"
    df = pd.read_csv(p)
    if "country" not in df.columns:
        return
    for c in ["observed_rate_per_1000", "rate_standardized_to_brazil_distribution", "rate_standardized_to_us_distribution"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    out = pd.DataFrame(
        {
            "Country": df["country"],
            "Outcome": df["outcome"].map(outcome_label),
            "Observed distribution rate per 1,000": df["observed_rate_per_1000"].map(lambda x: f"{x:.1f}"),
            "Rate if Brazil risk-profile distribution per 1,000": df["rate_standardized_to_brazil_distribution"].map(
                lambda x: f"{x:.1f}"
            ),
            "Rate if US risk-profile distribution per 1,000": df["rate_standardized_to_us_distribution"].map(
                lambda x: f"{x:.1f}"
            ),
            "Absolute change if Brazil distribution per 1,000": (
                df["rate_standardized_to_brazil_distribution"] - df["observed_rate_per_1000"]
            ).map(lambda x: f"{x:+.1f}"),
            "Absolute change if US distribution per 1,000": (
                df["rate_standardized_to_us_distribution"] - df["observed_rate_per_1000"]
            ).map(lambda x: f"{x:+.1f}"),
        }
    )
    out["Note"] = (
        "Descriptive standardization only. Standardized rate = sum(profile prevalence x profile-specific adjusted risk)."
    )
    out.to_csv(p, index=False)


def main():
    make_table1()
    legacy_main_tables = [
        (MAIN_DIR / "table2_singleton_outcome_rates.csv", make_table2),
        (MAIN_DIR / "table3_singleton_risk_score_models.csv", make_table3),
        (MAIN_DIR / "table4_singleton_phenotype_models.csv", make_table4),
        (MAIN_DIR / "table4_singleton_risk_profile_models.csv", make_table4),
        (MAIN_DIR / "table5_absolute_risks_with_ci.csv", make_table5),
    ]
    called = set()
    for path, fn in legacy_main_tables:
        if path.exists() and fn not in called:
            fn()
            called.add(fn)
        elif fn not in called:
            print(f"Skipping legacy main-table formatter; missing {path.name}.")
    make_supp1()
    make_supp2()
    make_supp4_5_7_8()
    make_supp6()
    make_supp9()
    make_supp10()
    print("Revision 7 table formatting complete.")


if __name__ == "__main__":
    main()
