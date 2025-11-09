use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde_json::json;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ProxyError {
    #[error("Invalid request format: {0}")]
    InvalidRequest(String),
    
    #[error("Backend error: {0}")]
    BackendError(#[from] reqwest::Error),
    
    #[error("Conversion error: {0}")]
    ConversionError(String),
    
    #[error("Configuration error: {0}")]
    ConfigError(String),
    
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

impl IntoResponse for ProxyError {
    fn into_response(self) -> Response {
        let (status, error_message) = match self {
            ProxyError::InvalidRequest(_) => (StatusCode::BAD_REQUEST, "Invalid request format"),
            ProxyError::BackendError(_) => (StatusCode::BAD_GATEWAY, "Backend service error"),
            ProxyError::ConversionError(_) => (StatusCode::INTERNAL_SERVER_ERROR, "Request conversion failed"),
            ProxyError::ConfigError(_) => (StatusCode::INTERNAL_SERVER_ERROR, "Configuration error"),
            ProxyError::SerializationError(_) => (StatusCode::INTERNAL_SERVER_ERROR, "Serialization error"),
        };

        let body = Json(json!({
            "error": {
                "code": status.as_u16(),
                "message": error_message,
                "details": self.to_string()
            }
        }));

        (status, body).into_response()
    }
}