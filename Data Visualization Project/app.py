import dash
from dash import dcc
from dash import html
from dash.dependencies import Input, Output
import plotly.graph_objects as go
import plotly.express as px
import pandas as pd
import numpy as np
from scipy import stats

# our imports
from text_samples import BARRA_LATERAL, STACKED_BARCHART, SALES_PROFIT, YEAR_PROFIT, SUNBURST, CLIENT_BEHAVIOUR, \
    PIE_CHART, MAP, SOURCES, AUTHORS

# Dataset loading
df = pd.read_csv('superstore.csv', encoding="ISO-8859-1")

# Dataset 'Processing'

df['Year'] = pd.to_datetime(df['Ship Date']).dt.year  # added a year column for easier data sorting
df.drop(df.index[df['Year'] == 2018], inplace=True)  # 2018 has incomplete data, so we drop the whole year

sub_categories = ['Bookcases', 'Chairs', 'Labels', 'Tables', 'Storage',
                  'Furnishings', 'Art', 'Phones', 'Binders', 'Appliances', 'Paper',
                  'Accessories', 'Envelopes', 'Fasteners', 'Supplies', 'Machines',
                  'Copiers']

TREECOLORS = ['#fdca26', '#bd3786', '#0d0887']
TREECOLORS_SUNBURST = ['#fdca26', '#0d0887', '#bd3786']

# State name to code
us_state_to_abbrev = {
    "Alabama": "AL",
    "Alaska": "AK",
    "Arizona": "AZ",
    "Arkansas": "AR",
    "California": "CA",
    "Colorado": "CO",
    "Connecticut": "CT",
    "Delaware": "DE",
    "Florida": "FL",
    "Georgia": "GA",
    "Hawaii": "HI",
    "Idaho": "ID",
    "Illinois": "IL",
    "Indiana": "IN",
    "Iowa": "IA",
    "Kansas": "KS",
    "Kentucky": "KY",
    "Louisiana": "LA",
    "Maine": "ME",
    "Maryland": "MD",
    "Massachusetts": "MA",
    "Michigan": "MI",
    "Minnesota": "MN",
    "Mississippi": "MS",
    "Missouri": "MO",
    "Montana": "MT",
    "Nebraska": "NE",
    "Nevada": "NV",
    "New Hampshire": "NH",
    "New Jersey": "NJ",
    "New Mexico": "NM",
    "New York": "NY",
    "North Carolina": "NC",
    "North Dakota": "ND",
    "Ohio": "OH",
    "Oklahoma": "OK",
    "Oregon": "OR",
    "Pennsylvania": "PA",
    "Rhode Island": "RI",
    "South Carolina": "SC",
    "South Dakota": "SD",
    "Tennessee": "TN",
    "Texas": "TX",
    "Utah": "UT",
    "Vermont": "VT",
    "Virginia": "VA",
    "Washington": "WA",
    "West Virginia": "WV",
    "Wisconsin": "WI",
    "Wyoming": "WY",
    "District of Columbia": "DC",
    "American Samoa": "AS",
    "Guam": "GU",
    "Northern Mariana Islands": "MP",
    "Puerto Rico": "PR",
    "United States Minor Outlying Islands": "UM",
    "U.S. Virgin Islands": "VI",
}

df['State'] = df['State'].map(lambda x: us_state_to_abbrev[x])

plot = ['Violin', 'Boxplot']

sub_categories_options = [dict(label=sub_category.replace('_', ' '), value=sub_category) for sub_category in
                          sub_categories]

plot_options = [dict(label=sub_category.replace('_', ' '), value=sub_category) for sub_category in plot]

dropdown_sub_category = dcc.Dropdown(
    id='sub_category_option',
    options=sub_categories_options,
    value='Bookcases',
    style={'font-family': 'arial'}
)

radio_projection = dcc.RadioItems(
    id='projection',
    options=[dict(label='Violin Plot', value=0,),
             dict(label='Box Plot', value=1)],
    value=0,
    style={'font-family': 'arial'}
)



# list of state codes

states_codes = sorted(df['State'].unique())

################################################RadioitemComponent#############################################################

sub_categories_options = [dict(label=sub_category.replace('_', ' '), value=sub_category) for sub_category in
                          sub_categories]

radio_interaction = dcc.RadioItems(
    id='interaction',
    options=sub_categories_options,
    value='Bookcases',
    labelStyle={'display': 'block'}
)

# Sales and Profit chart
df_3bar = df.filter(['Sub-Category', 'Sales', 'Discount', 'Profit'], axis=1)
df_3bar['DiscountMoney'] = df_3bar['Discount'] * df_3bar['Sales']
# del df_3bar['Discount']
# df_3bar.rename(columns={'DiscountMoney':'Discount'}, inplace=True)
df_graph = df_3bar.groupby(['Sub-Category']).sum().reset_index().round(2)

# Lineplot profit by category by year
df_lineplot = df.filter(['Year', 'Category', 'Profit'], axis=1)
df_lineorganized = df_lineplot.groupby(['Year', 'Category'], as_index=False)['Profit'].sum()
dummies = pd.get_dummies(df_lineorganized['Category']).mul(df_lineorganized.Profit, 0)
dummies['Year'] = df_lineorganized['Year']
df_linegraph = dummies.groupby(['Year']).sum().reset_index().round({'Furniture': 2, 'Office Supplies': 2, 'Technology': 2})

# Building our Graphs (nothing new here)
sales_profit_fig = px.bar(df_graph.round({'Sales': 2, 'Profit': 2}), x="Sub-Category", y=["Sales", "Profit"], barmode="group", color_discrete_sequence=TREECOLORS).update_layout(
    legend_title="Type")
lineplot_profit_fig = px.line(df_linegraph.round({'Furniture': 2, 'Office Supplies': 2, 'Technology': 2}), x="Year", y=['Furniture', 'Office Supplies', 'Technology'], markers=True, color_discrete_sequence=TREECOLORS).update_xaxes(
    dtick=1).update_layout(legend_title="Category")
sales_profit_fig.update_traces(yhoverformat = '.2f')
lineplot_profit_fig.update_traces(yhoverformat = '.2f')

df_stacked = df.round({'Profit': 2})
stacked = px.histogram(df_stacked, x="Segment", y="Profit", color="Category", hover_data=['Segment'], barmode='stack', color_discrete_sequence=TREECOLORS)
stacked.update_traces(yhoverformat = '.2f')

pie = px.pie(df, values='Profit', names='Segment', color_discrete_sequence=TREECOLORS)

df['DiscountMoney'] = df['Discount'] * df['Sales']
sunburst = px.sunburst(df.round({'DiscountMoney': 2}), path=['Category', 'Sub-Category'], values='DiscountMoney', color_discrete_sequence=TREECOLORS_SUNBURST)

# style={'float': 'right', 'width': '75%'}
# style={'float': 'left', 'width': '24%'})

# The App itself
app = dash.Dash(__name__)
server = app.server
app.layout = html.Div([
    html.Div([
        html.Div([
            html.Img(
                src=app.get_asset_url('nova-ims.png'),
                id='Nova-image',
                style={
                    'height': '80px',
                    'width': 'auto',
                    'float': 'left',
                    'display': 'inline-block',
                    'margin-left': '2%',
                },
            )
        ], style={'width': '30.6666666667%'}),
        html.Div([
            html.H2(children='Supply Chain Delivery Process Analysis', style={'text-align': 'center', 'font-family': 'arial', 'font-weight': 'bold'}),
            html.H3(children='Analysis of the Delivery Process of a Large Retail Company', style={'text-align': 'center', 'font-family': 'arial', 'font-weight': 'normal'})
        ])
        ], style={'margin-bottom': '2%', 'margin-top': '2%'}),
        html.Div([
            html.Div([
                html.Div([
                    html.Div([
                        html.H4('Project Overview', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top', 'margin-top': '2%', 'margin-bottom': '4%', 'font-weight': 'bold'}),
                        html.Div([BARRA_LATERAL, html.Br()], style={'text-align': 'justify', 'white-space': 'pre-wrap', 'font-family': 'arial', 'margin-left': '1%', 'margin-left': '1%'})], className="pretty_container", style={'float': 'left', 'width': '19%', 'display': 'inline-block', 'vertical-align' :'top'}),
                    html.Div([
                        html.Div([MAP, html.Label('Product Sub-Categories'), radio_interaction],
                             style={'width': "20%", 'font-family': 'arial', 'display': 'inline-block', 'vertical-align' :'top', 'white-space': 'pre-wrap'}, className="pretty_container"),
                        html.Div([dcc.Graph(id='choropleth_graph')], style={'width': "70%", 'display': 'inline-block', 'text-align': 'right'}, className="pretty_container")
                    ], className="row pretty_container", style={'float': 'right', 'width': '73%', 'display': 'inline-block', 'vertical-align' :'top'})
                    ], style={'display': 'inline-block'}),
                html.Div([
                    html.H3('Sales and Profit per Sub-Category', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align':'top'}),
                    html.Div([html.Div([SALES_PROFIT],
                                       style={'text-align': 'justify', 'white-space': 'pre-wrap', 'font-family': 'arial',
                                              'margin-left': '1%', 'margin-left': '1%', 'vertical-align' :'top'})]),
                    dcc.Graph(id='bar_charts', figure=sales_profit_fig, className="pretty_container")
                ], className="row pretty_container"),
                html.Div([
                    html.Div([
                        html.Div([
                            html.Div([
                                html.H3('Yearly Profit per Category', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                                html.Div([html.Div([YEAR_PROFIT], style={'text-align': 'justify', 'white-space': 'pre-wrap',
                                                                         'font-family': 'arial',
                                                                         'margin-left': '1%', 'vertical-align' :'top'})]),
                                html.Br(),
                                dcc.Graph(id='g1', figure=lineplot_profit_fig, style={'margin-right': '1%'})
                            ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block'}),
                            html.Div([
                                html.H3('Total Discounts per Category and Sub-Category', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                                html.Div([html.Div([SUNBURST], style={'text-align': 'justify', 'white-space': 'pre-wrap',
                                                                      'font-family': 'arial', 'margin-left': '1%', 'vertical-align' :'top'})]),
                                html.Br(),
                                dcc.Graph(id='g2', figure=sunburst, style={'margin-left': '1%'})
                            ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block', 'text-align': 'right'})], className="row pretty_container")]),
                    html.Div([
                        html.Div([
                            html.Div([
                                html.H3('Client Behaviour', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                                html.Div([html.Div([CLIENT_BEHAVIOUR],
                                                   style={'text-align': 'justify', 'white-space': 'pre-wrap',
                                                          'font-family': 'arial', 'margin-left': '1%',
                                                          'margin-left': '1%', 'vertical-align' :'top'})])
                            ]),
                            html.Div([
                                html.Div([
                                    html.H3('Profit by Client Type', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                                    html.Div([html.Div([STACKED_BARCHART],
                                                       style={'text-align': 'justify', 'white-space': 'pre-wrap',
                                                              'font-family': 'arial',
                                                              'margin-left': '1%', 'vertical-align' :'top'})]),
                                    html.Br(),
                                    html.Br(),
                                    html.Br(),
                                    html.Br(),
                                    html.Br(),
                                    html.Br(),
                                    dcc.Graph(id='g3', figure=stacked, style={'margin-right': '1%'})
                                ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block'}),
                                html.Div([
                                    html.H3('Sales of each Sub-Category by Client Type', style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                                    html.Div([html.Div([PIE_CHART],
                                                       style={'text-align': 'justify', 'white-space': 'pre-wrap',
                                                              'font-family': 'arial', 'margin-left': '1%', 'vertical-align' :'top'})]),
                                    html.P("Select the Sub-Category", style={"text-align": "center", "font-weight": "bold", 'font-family': 'arial'}),
                                    html.Div([
                                        dropdown_sub_category
                                    ], style={'text-align': 'left'}),
                                    html.Div([
                                        radio_projection
                                    ], style={'text-align': 'center'}),
                                    dcc.Graph(id="boxes")
                                ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block', 'text-align': 'right'}),
                            ], className="row pretty_container")])
                    ], className="row pretty_container"),
                    html.Div([
                        html.Div([
                            html.H3("Authors", style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                            html.Div([
                                AUTHORS
                            ], style={'text-align': 'left', 'white-space': 'pre-wrap', 'font-family': 'arial', 'vertical-align' :'top'}),
                            html.Br(),
                            html.Br(),
                            html.Br(),
                            html.Br(),
                            html.Br()
                        ])
                    ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block'}),
                    html.Div([
                        html.Div([
                            html.H3("Sources", style={'text-align': 'center', 'font-family': 'arial', 'vertical-align' :'top'}),
                            html.Div([
                                SOURCES[0],
                                html.A(SOURCES[1], href=SOURCES[1]),
                                SOURCES[2],
                                html.A(SOURCES[3], href=SOURCES[3]),
                                SOURCES[4],
                                html.A(SOURCES[5], href=SOURCES[5]),
                                SOURCES[6],
                                html.A(SOURCES[7], href=SOURCES[7]),
                                SOURCES[8],
                                html.A(SOURCES[9], href=SOURCES[9]),
                            ], style={'text-align': 'left', 'white-space': 'pre-wrap', 'font-family': 'arial', 'vertical-align' :'top'})
                        ])
                    ], className="row pretty_container", style={'width': '46%', 'display': 'inline-block', 'text-align': 'right'})
                ])
            ])

        ])
])

##############################################EXISTENCE IS PAIN###################################################################

# Building Choropleth Graph
@app.callback(Output('choropleth_graph', 'figure'),Input('interaction', 'value'))

def plot_chropleth(subgroup):
    # I have to query the data to get only the sub_categories I want!!!!!

    df_map = df.groupby(["Sub-Category", "State"]).sum("Profit")["Profit"].round(2)

    d = dict.fromkeys(states_codes, 0)
    z = dict(df_map[subgroup])

    for key in z:
        d[key] = z[key]
    result = pd.Series(d)

    data_choropleth = dict(type='choropleth',
                           locations=states_codes,
                           # There are three ways to 'merge' your data with the data pre embedded in the map
                           locationmode='USA-states',
                           z=result.astype(float),
                           colorscale='RdBu',
                           colorbar=dict(title='Profit'),
                           zmid=0
                           )

    layout_choropleth = dict(geo=dict(scope='usa'),
                             title=dict(text='Profit Value per US state',
                                        x=.5  # Title relative position according to the xaxis, range (0,1)
                                        )
                             )

    fig = go.Figure(data=data_choropleth, layout=layout_choropleth)
    return fig


@app.callback(Output('boxes', 'figure'),[Input("projection", "value"), Input("sub_category_option", "value")])

def plot_violin(plot_type, sub_category):
    OUTLIER_FENCE = 3

    cons = df.loc[(df["Segment"] == "Consumer") & (df["Sub-Category"] == sub_category) & (np.abs(stats.zscore(df['Sales'])) < OUTLIER_FENCE)]["Sales"].round(2)
    corp = df.loc[(df["Segment"] == "Corporate") & (df["Sub-Category"] == sub_category) & (np.abs(stats.zscore(df['Sales'])) < OUTLIER_FENCE)]["Sales"].round(2)
    home = df.loc[(df["Segment"] == "Home Office") & (df["Sub-Category"] == sub_category) & (np.abs(stats.zscore(df['Sales'])) < OUTLIER_FENCE)]["Sales"].round(2)

    trace0 = go.Box(
        y=cons,
        name="Consumer"
    )

    trace1 = go.Box(
        y=corp,
        name="Corporate"
    )

    trace2 = go.Box(
        y=home,
        name="Home Office"
    )
    if plot_type == 0:
        trace0 = go.Violin(
            y=cons,
            name="Consumer"
        )

        trace1 = go.Violin(
            y=corp,
            name="Corporate"
        )

        trace2 = go.Violin(
            y=home,
            name="Home Office"
        )

    data = [trace0, trace1, trace2]
    layout = go.Layout(title='Distribution of sales by customer type and Sub-Category')

    fig = go.Figure(data=data, layout=layout)
    return fig


##############################################callback#############################################################################

##############################################Graph################################################################################

if __name__ == '__main__':
    app.run_server(debug=True)


