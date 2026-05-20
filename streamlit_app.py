import streamlit as st
import pandas as pd
import plotly.express as px
APP_TITLE = "Canadian FDI GDP Time Series Dashboard"

st.set_page_config(page_title=APP_TITLE, layout="wide")
st.markdown('''
<style>
.block-container {padding-top: 1.5rem; padding-bottom: 2rem; max-width: 1180px;}
[data-testid="stMetricValue"] {font-size: 1.65rem;}
.small-note {color: #5f6368; font-size: 0.92rem;}
</style>
''', unsafe_allow_html=True)


df = pd.read_csv("data/fdi_gdp_sample.csv")
df["fdi_gdp_ratio"] = df.fdi_billion_usd / df.gdp_billion_usd * 100
st.title(APP_TITLE)
st.caption("Economic time-series dashboard inspired by the original R VECM and forecasting workflow.")
st.metric("Latest FDI", f"${df.fdi_billion_usd.iloc[-1]:.1f}B")
st.metric("Latest GDP", f"${df.gdp_billion_usd.iloc[-1]:.0f}B")
st.plotly_chart(px.line(df, x="year", y=["fdi_billion_usd", "gdp_billion_usd"], markers=True), use_container_width=True)
st.plotly_chart(px.bar(df, x="year", y="fdi_gdp_ratio", color="top_sector"), use_container_width=True)
st.dataframe(df, use_container_width=True, hide_index=True)
