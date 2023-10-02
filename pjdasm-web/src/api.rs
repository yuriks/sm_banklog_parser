use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use axum::{Json, Router};
use pjdasm::Bank;
use serde::{Deserialize, Serialize};
use serde_json::json;
use serde_with::DisplayFromStr;

use crate::model::{get_bank_for_display, AppState, DisplayLine};

pub fn routes_api() -> Router {
    println!("Loading initial state...");
    let state = Arc::new(AppState::new());
    println!("Initial load finished!");

    Router::new()
        .route("/bank/:bank", get(handler_get_bank)) // TODO: HEAD route for performance?
        .with_state(state)
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Bank ${bank:02X} doesn't exist or is invalid")]
    BankNotFound { bank: Bank },

    #[error("Unknown Error")]
    Unknown(#[from] anyhow::Error),
}

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        let (http_status, error_id) = match self {
            Error::BankNotFound { .. } => (StatusCode::NOT_FOUND, "BANK_NOT_FOUND"),
            Error::Unknown(..) => (StatusCode::INTERNAL_SERVER_ERROR, "UNKNOWN_ERROR"),
        };

        let json_err = Json(json!({
            "http_status": http_status.as_u16(),
            "type": error_id,
            "description": self.to_string(),
        }));
        (http_status, json_err).into_response()
    }
}

#[derive(Deserialize)]
struct GetBankParams {
    #[serde(with = "serde_with::As::<DisplayFromStr>")]
    bank: Bank,
}

#[derive(Serialize)]
struct GetBankResult {
    lines: Vec<DisplayLine>,
}

#[axum::debug_handler]
async fn handler_get_bank(
    State(state): State<Arc<AppState>>,
    Path(params): Path<GetBankParams>,
) -> Result<Json<GetBankResult>> {
    let processed = state.processed.load();

    let display_lines = get_bank_for_display(params.bank, &state.persistent, &*processed)?;

    Ok(Json(GetBankResult {
        lines: display_lines,
    }))
}
